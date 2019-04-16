#include "form/LetStarWorker.hpp"

#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "form/LetStar.hpp"
#include "form/LetStarCont.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

LetStarWorker::LetStarWorker(ScamExpr * args,
                             Continuation * cont,
                             Env * env,
                             ScamEngine * engine)
    : LetBaseWorker("LetStar", args, cont, env)
    , engine(engine)
{
}

LetStarWorker * LetStarWorker::makeInstance(ScamExpr * args,
                                            Continuation * cont,
                                            Env * env,
                                            ScamEngine * engine)
{
    return new LetStarWorker(args, cont, env, engine);
}

void LetStarWorker::do_next(ScamExpr * formals,
                            ScamExpr * values,
                            ScamExpr * forms)
{
    Env * extended = env->extend();
    ScamExpr * safe = LetStar::safeCons(values);

    Continuation * ch =
        standardMemoryManager.make<LetStarCont>(formals,
                                                safe->getCdr(),
                                                forms,
                                                cont,
                                                extended,
                                                engine);
    safe->getCar()->eval(ch, env);
}
