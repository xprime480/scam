
#include "form/Quote.hpp"

#include "Continuation.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Quote::Quote()
    : SpecialForm("quote")
{
}

Quote * Quote::makeInstance()
{
    static Quote quote;
    return &quote;
}

void Quote::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    ScamExpr * expr = args->getCar();
    cont->run(expr);
}
