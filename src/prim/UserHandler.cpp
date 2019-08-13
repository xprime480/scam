#include "prim/UserHandler.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ValueFactory.hpp"

using namespace scam;
using namespace std;

UserHandler::UserHandler(ScamValue handler,
                         Continuation * cont,
                         Env * env,
                         ScamEngine * engine)
    : Handler("User Handler")
    , handler(handler)
    , cont(cont)
    , env(env)
    , engine(engine)
{
}

UserHandler * UserHandler::makeInstance(ScamValue handler,
                                        Continuation * cont,
                                        Env * env,
                                        ScamEngine * engine)
{
    return new UserHandler(handler, cont, env, engine);
}

void UserHandler::mark()
{
    if ( ! isMarked() ) {
        Handler::mark();
        handler->mark();
        cont->mark();
        env->mark();
    }
}

ScamValue UserHandler::handleError(ScamValue err)
{
    engine->popHandler();
    ScamValue args = makeList(err);
    apply(handler, args, cont, env, engine);
    return makeNothing();
}
