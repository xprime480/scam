#include "form/EnvHelper.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "input/ArgParser.hpp"
#include "input/CountedListParser.hpp"

using namespace scam;
using namespace std;

EnvHelper::EnvHelper(char const * name, ScamEngine * engine)
    : SpecialForm(name, true)
    , engine(engine)
{
}

bool EnvHelper::checkArgs(ExprHandle args,
                          Continuation * cont,
                          bool exprNeeded)
{
    const size_t expected = 1u + (exprNeeded ? 1u : 0u);
    ArgParser * any = standardMemoryManager.make<ArgParser>();
    CountedListParser * parser =
        standardMemoryManager.make<CountedListParser>(any, expected, expected);

    if ( ! parser->accept(args) ) {
        ExprHandle err =
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
