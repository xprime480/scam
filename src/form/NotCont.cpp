#include "form/NotCont.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

NotCont::NotCont(Continuation * cont)
    : Continuation("Not")
    , cont(cont)
{
}

NotCont * NotCont::makeInstance(Continuation * cont)
{
    return new NotCont(cont);
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

    if ( error(expr) ) {
        cont->run(expr);
    }
    else {
        ScamValue rv =
            ExpressionFactory::makeBoolean(! truth(expr));
        cont->run(rv);
    }
}
