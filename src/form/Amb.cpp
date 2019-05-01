#include "form/Amb.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/AmbBacktracker.hpp"
#include "input/ListParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

Amb::Amb(ScamEngine * engine)
    : SpecialForm("amb", true)
    , engine(engine)
{
}

Amb * Amb::makeInstance(ScamEngine * engine)
{
    return new Amb(engine);
}

void Amb::apply(ExprHandle args, Continuation * cont, Env * env)
{
    ListParser * parser = getListOfAnythingParser();
    if ( ! parser->accept(args) ) {
        ExprHandle err =
            ExpressionFactory::makeError("amb expects (form...); got: ",
                                         args->toString());
        cont->run(err);
        return;
    }

    Backtracker * backtracker = engine->getBacktracker();
    Backtracker * newBt =
        standardMemoryManager.make<AmbBacktracker>(args,
                                                   cont,
                                                   env,
                                                   engine,
                                                   backtracker);
    newBt->run();
}
