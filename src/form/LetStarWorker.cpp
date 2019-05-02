#include "form/LetStarWorker.hpp"

#include "Env.hpp"
#include "expr/ScamExpr.hpp"
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

void LetStarWorker::do_next(ExprHandle formals,
                            ExprHandle values,
                            ExprHandle forms)
{
    Env * extended = env->extend();
    ExprHandle safe = LetStar::safeCons(values);

    Continuation * ch =
        standardMemoryManager.make<LetStarCont>(formals,
                                                safe->getCdr(),
                                                forms,
                                                cont,
                                                extended,
                                                engine);
    safe->getCar()->eval(ch, env);
}

