#include "form/NotCont.hpp"

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

void NotCont::mark() const
{
  if ( ! isMarked() ) {
      Continuation::mark();
      cont->mark();
  }
}

void NotCont::run(ScamValue expr)
{
    Continuation::run(expr);

    if ( isError(expr) ) {
        cont->run(expr);
    }
    else {
        ScamValue rv = makeBoolean(! truth(expr));
        cont->run(rv);
    }
}
