#include "form/EnvHelper.hpp"

#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

EnvHelper::EnvHelper(char const * name, ScamEngine * engine)
    : SpecialForm(name, true)
    , engine(engine)
{
}

bool EnvHelper::checkArgs(ScamExpr * args,
                          Continuation * cont,
                          bool exprNeeded)
{
    const size_t expected = 1u + (exprNeeded ? 1u : 0u);
    const size_t actual   = args->length();

    if ( expected != actual ) {
        ScamExpr * err =
            ExpressionFactory::makeError("Expecting ",
                                         expected,
                                         "forms for argument list; ",
                                         "got: ",
                                         args->toString());
        cont->run(err);
        return false;
    }

    return true;
}
