#include "form/NotCont.hpp"

#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

using namespace scam;
using namespace std;

NotCont::NotCont(Continuation * cont, ScamEngine * engine)
    : Continuation("Not", engine)
    , cont(cont)
{
}

NotCont * NotCont::makeInstance(Continuation * cont, ScamEngine * engine)
{
    return new NotCont(cont, engine);
}

void NotCont::mark()
{
  if ( ! isMarked() ) {
      Continuation::mark();
      cont->mark();
  }
}

void NotCont::handleValue(ScamValue value)
{
    Continuation::handleValue(value);

    if ( isError(value) ) {
        engine->handleError(value);
    }
    else {
        ScamValue rv = makeBoolean(! truth(value));
        cont->handleValue(rv);
    }
}
