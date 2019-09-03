#include "form/LetStarWorker.hpp"

#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "form/AllSpecialForms.hpp"
#include "form/LetStarCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

LetStarWorker::LetStarWorker(LetDef & def, Continuation * cont, Env * env)
    : LetBaseWorker("LetStar", def, cont, env)
{
}

LetStarWorker *
LetStarWorker::makeInstance(LetDef & def, Continuation * cont, Env * env)
{
    return new LetStarWorker(def, cont, env);
}

void
LetStarWorker::do_next(ScamValue formals, ScamValue values, ScamValue forms)
{
    Env * extended = env->extend();
    /* FIXME */
    ScamValue safe = safeCons(values);

    Continuation * ch =
        standardMemoryManager.make<LetStarCont>(formals,
                                                getCdr(safe),
                                                forms,
                                                cont,
                                                extended);
    eval(getCar(safe), ch, env);
}

