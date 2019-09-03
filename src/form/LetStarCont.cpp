#include "form/LetStarCont.hpp"

#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "form/AllSpecialForms.hpp"
#include "form/LetStarBacktracker.hpp"
#include "form/LetStarCont.hpp"
#include "util/MemoryManager.hpp"

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
        Continuation * ch =
            standardMemoryManager.make<LetStarCont>(getCdr(formals),
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
    Backtracker * backtracker = ScamEngine::getEngine().getBacktracker();
    Backtracker * newBT =
        standardMemoryManager.make<LetStarBacktracker>(env, sym, backtracker);
    ScamEngine::getEngine().setBacktracker(newBT);
}
