#include "form/LetStarCont.hpp"

#include "Env.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ScamSymbol.hpp"
#include "expr/TypePredicates.hpp"
#include "form/LetStar.hpp"
#include "form/LetStarBacktracker.hpp"
#include "form/LetStarCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

LetStarCont::LetStarCont(ScamValue formals,
                         ScamValue rest,
                         ScamValue forms,
                         Continuation * cont,
                         Env * env,
                         ScamEngine * engine)
    : LetCommonCont("Let*", forms, cont)
    , formals(formals)
    , rest(rest)
    , env(env)
    , engine(engine)
{
}

LetStarCont * LetStarCont::makeInstance(ScamValue formals,
                                        ScamValue rest,
                                        ScamValue forms,
                                        Continuation * cont,
                                        Env * env,
                                        ScamEngine * engine)
{
    return new LetStarCont(formals, rest, forms, cont, env, engine);
}

void LetStarCont::mark() const
{
    if ( ! isMarked() ) {
        LetCommonCont::mark();
        formals->mark();
        rest->mark();
        env->mark();
    }
}

void LetStarCont::do_let(ScamValue expr)
{
    if ( isNil(formals) ) {
        final_eval(env);
    }
    else {
        ScamEnvKeyType sym = dynamic_cast<ScamSymbol *>(formals->getCar());
        env->put(sym, expr);

        makeBacktracker(sym);

        ScamValue safe = LetStar::safeCons(rest);
        Continuation * ch =
            standardMemoryManager.make<LetStarCont>(formals->getCdr(),
                                                    safe->getCdr(),
                                                    forms,
                                                    cont,
                                                    env,
                                                    engine);
        safe->getCar()->eval(ch, env);
    }
}

void LetStarCont::makeBacktracker(ScamEnvKeyType sym) const
{
    Backtracker * backtracker = engine->getBacktracker();
    Backtracker * newBT =
        standardMemoryManager.make<LetStarBacktracker>(env,
                                                       sym,
                                                       backtracker);
    engine->setBacktracker(newBT);
}
