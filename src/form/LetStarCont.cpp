#include "form/LetStarCont.hpp"

#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/SequenceOps.hpp"
#include "form/AllSpecialForms.hpp"
#include "form/LetStarBacktracker.hpp"
#include "form/LetStarCont.hpp"
#include "util/MemoryManager.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

using namespace scam;
using namespace std;

LetStarCont::LetStarCont(ScamValue formals,
                         ScamValue rest,
                         ScamValue forms,
                         Continuation * cont,
                         Env * env)
    : LetCommonCont("Let*", forms, cont)
    , formals(formals)
    , rest(rest)
    , env(env)
{
}

LetStarCont * LetStarCont::makeInstance(ScamValue formals,
                                        ScamValue rest,
                                        ScamValue forms,
                                        Continuation * cont,
                                        Env * env)
{
    return new LetStarCont(formals, rest, forms, cont, env);
}

void LetStarCont::mark()
{
    if ( ! isMarked() ) {
        LetCommonCont::mark();
        formals->mark();
        rest->mark();
        env->mark();
    }
}

ScamValue LetStarCont::do_let(ScamValue expr)
{
    if ( isNull(formals) ) {
        final_eval(env);
    }
    else {
        ScamValue sym = getCar(formals);
        ScamValue test = env->put(sym, expr);
        if ( isError(test) ) {
            return test;
        }

        makeBacktracker(sym);

        ScamValue safe = safeCons(rest);
        MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
        Continuation * ch = mm.make<LetStarCont>(getCdr(formals),
                                                 getCdr(safe),
                                                 forms,
                                                 cont,
                                                 env);
        eval(getCar(safe), ch, env);
    }

    return makeNothing();
}

void LetStarCont::makeBacktracker(ScamValue sym) const
{
    ScamEngine & engine = ScamEngine::getEngine();
    MemoryManager & mm = engine.getMemoryManager();

    Backtracker * backtracker = engine.getBacktracker();
    Backtracker * newBT = mm.make<LetStarBacktracker>(env, sym, backtracker);
    engine.setBacktracker(newBT);
}
