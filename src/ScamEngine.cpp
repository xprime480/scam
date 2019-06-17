#include "ScamEngine.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "Trampoline.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
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
}

ScamEngine::ScamEngine()
    : env(nullptr)
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

    env = standardMemoryManager.make<Env>();
    if ( initEnv ) {
        getStandardEnv();
    }
}

void ScamEngine::pushFrame()
{
    env = env->extend();
}

Env * ScamEngine::getFrame()
{
    return env;
}

void ScamEngine::popFrame()
{
    if ( env != env->getTop() ) {
        env = env->getParent();
    }
}

void ScamEngine::addBinding(ScamValue key, ScamValue val)
{
    env->put(key, val);
}

bool ScamEngine::hasBinding(ScamValue key, bool checkParent)
{
    return env->check(key, checkParent);
}

ScamValue ScamEngine::getBinding(ScamValue key, bool top)
{
    Env * temp = top ? env->getTop() : env;
    return temp->get(key);
}

void ScamEngine::rebind(ScamValue key, ScamValue val)
{
    env->assign(key, val);
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

    while ( true ) {
        ScamValue expr = read();
        if ( isNothing(expr) ) {
            break;
        }
        (void) eval(expr);
    }

    ScamValue rv;
    if ( ! hc || hc->current() == mark ) {
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
    scam::eval(expr, cont, env, this);

    Trampoline(GlobalWorkQueue);

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
    ScamValue rv = err;
    if ( handlers.empty() ) {
        Handler handler;
        rv = handler.handleError(err);
    }
    else {
        rv = handlers.back()->handleError(err);
    }

    if ( isError(rv) ) {
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
    env->mark();
    if ( backtracker ) {
        backtracker->mark();
    }
    cont->mark();
    for ( auto & i : input ) {
        i.mark();
    }
    GlobalWorkQueue.mark();
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
}
