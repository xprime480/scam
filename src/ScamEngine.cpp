#include "ScamEngine.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"
#include "Trampoline.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "env/Env.hpp"
#include "env/EnvOps.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "input/ScamParser.hpp"
#include "util/MemoryManager.hpp"

using namespace std;
using namespace scam;

namespace
{
    class HistoryCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        HistoryCont(size_t size, ScamEngine * engine);

        static HistoryCont * makeInstance(size_t size, ScamEngine * engine);

    public:
        void mark() override;

        void setCont(Continuation * c);

        void handleValue(ScamValue value) override;

        ScamValue get(size_t which = 1) const;
        size_t current() const;

    private:
        size_t size;
        vector<ScamValue> history;
        Continuation * cont;
        size_t serial;
    };

    class EngineHandler : public Handler
    {
    private:
        friend class scam::MemoryManager;
        EngineHandler();
        static EngineHandler * makeInstance();

    public:
        void mark() override;

        ScamValue handleError(ScamValue err) override;

        bool called() const;
        ScamValue get() const;

    private:
        ScamValue value;
    };
}

Env * scam::configEnv  = nullptr;

ScamEngine::ScamEngine()
    : env(nullptr)
    , topEnv(nullptr)
    , libs(nullptr)
    , backtracker(nullptr)
    , cont(nullptr)
    , marker(this)
{
    standardMemoryManager.addHook(&marker);
}

ScamEngine::~ScamEngine()
{
    standardMemoryManager.removeHook(&marker);
}

void ScamEngine::reset(bool initEnv)
{
    backtracker = nullptr;
    cont = standardMemoryManager.make<HistoryCont>(1, this);

    input.clear();
    loaded.clear();
    handlers.clear();

    libs = makeDict();
    configEnv = getConfigurationEnv(this);
    topEnv = env = getSyntaxEnv(this, configEnv);
    initalizeLibraries(this, env);
    if ( initEnv ) {
        env = makeInteractionEnv(this, env);
    }
    topEnv = env = env->extend();

}

Env * ScamEngine::getFrame()
{
    return env;
}

Env * ScamEngine::getInteractionFrame()
{
    return topEnv;
}

void ScamEngine::setFrame(Env * newEnv)
{
    env = newEnv;
}

ScamValue ScamEngine::findLibrary(ScamValue name)
{
    if ( dictHas(libs, name) ) {
        return dictGet(libs, name);
    }

    return makeNothing();
}

void ScamEngine::saveLibrary(ScamValue name, Env * env)
{
    dictPut(libs, name, makeEnv(env));
}

void ScamEngine::pushInput(Tokenizer & tokenizer)
{
    input.emplace_back(ScamParser(tokenizer));
}

void ScamEngine::popInput()
{
    if ( ! input.empty() ) {
        input.pop_back();
    }
}

void ScamEngine::setCont(Continuation * c)
{
    HistoryCont * hc = dynamic_cast<HistoryCont *>(cont);
    if ( hc ) {
        hc->setCont(c);
    }
}

ScamValue ScamEngine::readEvalCurrent()
{
    HistoryCont const * hc = dynamic_cast<HistoryCont const *>(cont);
    size_t const mark = hc ? hc->current() : 0;

    EngineHandler * eh = standardMemoryManager.make<EngineHandler>();

    while ( true ) {
        ScamValue expr = read();
        if ( isNothing(expr) ) {
            break;
        }
        if ( isEof(expr) ) {
            break;
        }

        eval(expr, eh);
        if ( eh->called() ) {
            break;
        }
    }

    ScamValue rv;
    if ( eh->called() ) {
        rv = eh->get();
    }
    else if ( ! hc || hc->current() == mark ) {
        rv = makeNothing();
    }
    else {
        rv = hc->get();
    }

    return rv;
}

ScamValue ScamEngine::read()
{
    if ( input.empty() ) {
        return makeNothing();
    }

    ScamParser & p = input.back();
    return p.parseExpr();
}

ScamValue ScamEngine::eval(ScamValue expr)
{
    EngineHandler * eh = standardMemoryManager.make<EngineHandler>();
    ScamValue rv = eval(expr, eh);
    if ( eh->called() ) {
        rv = eh->get();
    }

    return rv;
}

ScamValue ScamEngine::eval(ScamValue expr, Handler * handler)
{
    pushHandler(handler);
    scam::eval(expr, cont, env, this);
    Trampoline(GlobalWorkQueue);
    popHandler();

    HistoryCont const * hc = dynamic_cast<HistoryCont const *>(cont);
    ScamValue rv = hc->get();
    return rv;
}

ScamValue ScamEngine::apply(ScamValue expr, ScamValue args)
{
    scam::apply(expr, args, cont, env, this);
    Trampoline(GlobalWorkQueue);
    HistoryCont const * hc = dynamic_cast<HistoryCont const *>(cont);
    return hc->get();
}

Backtracker * ScamEngine::getBacktracker()
{
    Backtracker * tmp = backtracker;
    backtracker = nullptr;
    return tmp;
}

void ScamEngine::setBacktracker(Backtracker * backtracker_in)
{
    backtracker = backtracker_in;
}

bool ScamEngine::isLoaded(std::string const & filename) const
{
    return loaded.find(filename) != loaded.end();
}

void ScamEngine::setLoaded(std::string const & filename)
{
    loaded.insert(filename);
}

void ScamEngine::pushHandler(Handler * handler)
{
    handlers.push_back(handler);
}

ScamValue ScamEngine::handleError(ScamValue err)
{
    if ( isError(err) ) {
        err->errorHandled() = true;
    }

    ScamValue rv = err;
    if ( handlers.empty() ) {
        Handler * handler = standardMemoryManager.make<Handler>();
        pushHandler(handler);
        rv = handler->handleError(err);
    }
    else {
        auto h = handlers.back();
        rv = h->handleError(err);
    }

    if ( isUnhandledError(rv) ) {
        reset(false);
        exit(1);                // unhandled errors kill the system
    }

    return rv;
}

void ScamEngine::popHandler()
{
    if ( ! handlers.empty() ) {
        handlers.pop_back();
    }
}

void ScamEngine::mark()
{
    configEnv->mark();
    env->mark();
    cont->mark();
    GlobalWorkQueue.mark();

    if ( backtracker ) {
        backtracker->mark();
    }

    for ( auto & i : input ) {
        i.mark();
    }

    for ( auto & h : handlers ) {
        h->mark();
    }

    libs->mark();
}

namespace
{
    HistoryCont::HistoryCont(size_t size, ScamEngine * engine)
        : Continuation("History", engine)
        , size(size)
        , cont(standardMemoryManager.make<Continuation>("Default", engine))
        , serial(0u)
    {
    }

    HistoryCont * HistoryCont::makeInstance(size_t size, ScamEngine * engine)
    {
        return new HistoryCont(size, engine);
    }

    void HistoryCont::mark()
    {
        if ( ! isMarked() ) {
            Continuation::mark();
            if ( cont ) {
                cont->mark();
            }
            for ( auto h : history ) {
                h->mark();
            }
        }
    }

    void HistoryCont::setCont(Continuation * c)
    {
        cont = c;
    }

    void HistoryCont::handleValue(ScamValue value)
    {
        Continuation::handleValue(value);

        cont->handleValue(value);

        history.push_back(value);
        ++serial;
        while ( history.size() > size ) {
            history.erase(history.begin());
        }
    }

    ScamValue HistoryCont::get(size_t which) const
    {
        if ( which == 0 || which > history.size() ) {
            return makeNothing();
        }

        auto p = history.rbegin();
        p -= (which - 1);
        return *p;
    }

    size_t HistoryCont::current() const
    {
        return serial;
    }

    EngineHandler::EngineHandler()
        : Handler("EngineHandler")
    {
        value = makeNothing();
    }

    EngineHandler * EngineHandler::makeInstance()
    {
        return new EngineHandler;
    }

    void EngineHandler::mark()
    {
        if ( ! isMarked() ) {
            Handler::mark();
            value->mark();
        }
    }

    ScamValue EngineHandler::handleError(ScamValue err)
    {
        Handler::handleError(err);
        value = err;
        return makeNothing();
    }

    bool EngineHandler::called() const
    {
        return ! isNothing(value);
    }

    ScamValue EngineHandler::get() const
    {
        return value;
    }
}
