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
        HistoryCont(size_t size);

        static HistoryCont * makeInstance(size_t size);

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

ScamEngine::ScamEngine()
    : configEnv(nullptr)
    , env(nullptr)
    , topEnv(nullptr)
    , libs(nullptr)
    , backtracker(nullptr)
    , cont(nullptr)
{
    mm.addHook(&marker);
    initializeValueFactory(mm);
}

ScamEngine::~ScamEngine()
{
    mm.removeHook(&marker);
}

ScamEngine & ScamEngine::getEngine()
{
    static ScamEngine engine;
    return engine;
}

void ScamEngine::reset(bool initEnv)
{
    mm.addHook(&marker);
    initializeValueFactory(mm);

    backtracker = nullptr;
    cont = mm.make<HistoryCont>(1);

    input.clear();
    loaded.clear();
    handlers.clear();

    libs = makeDict();
    configEnv = getConfigurationEnv();
    topEnv = env = getSyntaxEnv(configEnv);
    initalizeLibraries(env);
    if ( initEnv ) {
        env = makeInteractionEnv(env);
    }
    topEnv = env = env->extend();
}

Env * ScamEngine::getFrame()
{
    return env;
}

Env * ScamEngine::getConfigFrame()
{
    return configEnv;
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

    EngineHandler * eh = mm.make<EngineHandler>();

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
    EngineHandler * eh = mm.make<EngineHandler>();
    ScamValue rv = eval(expr, eh);
    if ( eh->called() ) {
        rv = eh->get();
    }

    return rv;
}

ScamValue ScamEngine::eval(ScamValue expr, Handler * handler)
{
    bool suppressed = mm.isSuppressed();
    mm.setSuppressed(true);

    pushHandler(handler);
    scam::eval(expr, cont, env);
    Trampoline(GlobalWorkQueue);
    popHandler();

    mm.setSuppressed(suppressed);
    mm.gc();

    HistoryCont const * hc = dynamic_cast<HistoryCont const *>(cont);
    ScamValue rv = hc->get();
    return rv;
}

ScamValue ScamEngine::apply(ScamValue expr, ScamValue args)
{
    scam::apply(expr, args, cont, env);
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
        Handler * handler = mm.make<Handler>();
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

MemoryManager & ScamEngine::getMemoryManager()
{
    return mm;
}

void ScamEngine::release()
{
    configEnv = nullptr;
    // GlobalWorkQueue.clear();

    env = nullptr;
    topEnv = nullptr;
    libs = nullptr;

    input.clear();
    backtracker = nullptr;
    cont = nullptr;

    handlers.clear();
}

void ScamEngine::mark()
{
    configEnv->mark();
    GlobalWorkQueue.mark();

    env->mark();
    topEnv->mark();
    libs->mark();

    for ( auto & i : input ) {
        i.mark();
    }

    if ( backtracker ) {
        backtracker->mark();
    }

    cont->mark();

    for ( auto & h : handlers ) {
        h->mark();
    }
}

namespace
{
    HistoryCont::HistoryCont(size_t size)
        : Continuation("History")
        , size(size)
        , cont(nullptr)
        , serial(0u)
    {
        ScamEngine & engine = ScamEngine::getEngine();
        cont = engine.getMemoryManager().make<Continuation>("Default");
    }

    HistoryCont * HistoryCont::makeInstance(size_t size)
    {
        return new HistoryCont(size);
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
