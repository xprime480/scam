#include "ScamEngine.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "Trampoline.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "input/ScamParser.hpp"
#include "util/MemoryManager.hpp"

#include <iostream>

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
        void mark() const override;

        void setCont(Continuation * c);

        void run(ScamExpr * expr) override;

        ScamExpr * get(size_t which = 1) const;
        size_t current() const;

    private:
        size_t size;
        vector<ScamExpr *> history;
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
    cont = standardMemoryManager.make<HistoryCont>(1);

    input.clear();
    loaded.clear();

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

void ScamEngine::addBinding(ScamExpr * key, ScamExpr * val)
{
    env->put(key, val);
}

bool ScamEngine::hasBinding(ScamExpr * key, bool checkParent)
{
    return env->check(key, checkParent);
}

ScamExpr * ScamEngine::getBinding(ScamExpr * key, bool top)
{
    Env * temp = top ? env->getTop() : env;
    return temp->get(key);
}

void ScamEngine::rebind(ScamExpr * key, ScamExpr * val)
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

ScamExpr * ScamEngine::parseCurrentInput()
{
    HistoryCont const * hc = dynamic_cast<HistoryCont const *>(cont);
    size_t const mark = hc ? hc->current() : 0;

    while ( true ) {
        ScamExpr * expr = read();
        if ( expr->isNull() ) {
            break;
        }
        eval(expr);
    }

    if ( ! hc || hc->current() == mark ) {
        return ExpressionFactory::makeNull();
    }
    ScamExpr * rv = hc->get();
    return rv;
}

ScamExpr * ScamEngine::read()
{
    if ( input.empty() ) {
        return ExpressionFactory::makeNull();
    }

    ScamParser & p = input.back();
    return p.parseExpr();
}

ScamExpr * ScamEngine::eval(ScamExpr * expr)
{
    expr->eval(cont, env);

    Trampoline(GlobalWorkQueue);

    HistoryCont const * hc = dynamic_cast<HistoryCont const *>(cont);
    ScamExpr * rv = hc->get();
    return rv;
}

ScamExpr * ScamEngine::apply(ScamExpr * expr, ScamExpr * args)
{
    expr->apply(args, cont, env);
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

void ScamEngine::mark() const
{
    env->mark();
    if ( backtracker ) {
	backtracker->mark();
    }
    cont->mark();
    GlobalWorkQueue.mark();
}

namespace
{
    HistoryCont::HistoryCont(size_t size)
        : Continuation("History")
        , size(size)
        , cont(standardMemoryManager.make<Continuation>("Default"))
        , serial(0u)
    {
    }

    HistoryCont * HistoryCont::makeInstance(size_t size)
    {
        return new HistoryCont(size);
    }

    void HistoryCont::mark() const
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

    void HistoryCont::run(ScamExpr * expr)
    {
        Continuation::run(expr);

        cont->run(expr);

        history.push_back(expr);
        ++serial;
        while ( history.size() > size ) {
            history.erase(history.begin());
        }
    }

    ScamExpr * HistoryCont::get(size_t which) const
    {
        if ( which == 0 || which > history.size() ) {
            return ExpressionFactory::makeNull();
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
