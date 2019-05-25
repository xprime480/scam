#include "form/LetStarWorker.hpp"

#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/SequenceOps.hpp"
#include "form/LetStar.hpp"
#include "form/LetStarCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

LetStarWorker::LetStarWorker(LetParser * parser,
                             Continuation * cont,
                             Env * env,
                             ScamEngine * engine)
    : LetBaseWorker("LetStar", parser, cont, env)
    , engine(engine)
{
}

LetStarWorker * LetStarWorker::makeInstance(LetParser * parser,
                                            Continuation * cont,
                                            Env * env,
                                            ScamEngine * engine)
{
    return new LetStarWorker(parser, cont, env, engine);
}

void LetStarWorker::do_next(ScamValue formals,
                            ScamValue values,
                            ScamValue forms)
{
    Env * extended = env->extend();
    ScamValue safe = LetStar::safeCons(values);

    Continuation * ch =
        standardMemoryManager.make<LetStarCont>(formals,
                                                getCdr(safe),
                                                forms,
                                                cont,
                                                extended,
                                                engine);
    getCar(safe)->eval(ch, env);
}

