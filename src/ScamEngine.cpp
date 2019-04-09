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
    public:
        HistoryCont(size_t size);

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
{
}

void ScamEngine::reset(bool initEnv)
{
    cont = make_shared<HistoryCont>(1);

    loaded.clear();

    env.reset();
    env = env.top();

    if ( initEnv ) {
        getStandardEnv();
    }
}

void ScamEngine::pushFrame()
{
    env = env.extend();
}

Env ScamEngine::getFrame()
{
    return env;
}

void ScamEngine::popFrame()
{
    env = env.parent();
}

void ScamEngine::addBinding(ScamExpr * key, ScamExpr * val)
{
    env.put(key, val);
}

bool ScamEngine::hasBinding(ScamExpr * key, bool checkParent)
{
    return env.check(key, checkParent);
}

ScamExpr * ScamEngine::getBinding(ScamExpr * key, bool top)
{
    Env temp = top ? env.top() : env;
    return temp.get(key);
}

void ScamEngine::rebind(ScamExpr * key, ScamExpr * val)
{
    env.assign(key, val);
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

void ScamEngine::setCont(ContHandle c)
{
    HistoryCont * hc = dynamic_cast<HistoryCont *>(cont.get());
    if ( hc ) {
        hc->setCont(c.get());
    }
}

ScamExpr * ScamEngine::parseCurrentInput()
{
    HistoryCont const * hc = dynamic_cast<HistoryCont const *>(cont.get());
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

    HistoryCont const * hc = dynamic_cast<HistoryCont const *>(cont.get());
    ScamExpr * rv = hc->get();
    return rv;
}

ScamExpr * ScamEngine::apply(ScamExpr * expr, ScamExpr * args)
{
    expr->apply(args, cont, env);
    Trampoline(GlobalWorkQueue);
    HistoryCont const * hc = dynamic_cast<HistoryCont const *>(cont.get());
    return hc->get();
}

BacktrackHandle ScamEngine::getBacktracker()
{
    BacktrackHandle tmp = backtracker;
    backtracker.reset();
    return tmp;
}

void ScamEngine::setBacktracker(BacktrackHandle backtracker_in)
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

namespace
{
    HistoryCont::HistoryCont(size_t size)
        : Continuation("History")
        , size(size)
        , cont(standardMemoryManager.make<Continuation>("Default"))
        , serial(0u)
    {
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
