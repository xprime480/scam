
#include "form/Amb.hpp"

#include "ScamEngine.hpp"
#include "form/AmbBacktracker.hpp"
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

void Amb::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    Backtracker * backtracker = engine->getBacktracker();
    Backtracker * newBt =
        standardMemoryManager.make<AmbBacktracker>(args,
                                                   cont,
                                                   env,
                                                   engine,
                                                   backtracker);
    newBt->run();
}
