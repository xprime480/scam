
#include "form/Quote.hpp"

#include "Continuation.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

Quote::Quote()
    : SpecialForm("quote")
{
}

void Quote::apply(std::shared_ptr<ScamExpr> const & args,
                  std::shared_ptr<Continuation> cont,
                  Env & env)
{
    shared_ptr<ScamExpr> expr = args->getCar();
    cont->run(expr);
}

std::shared_ptr<ScamExpr> Quote::clone()
{
    return ExpressionFactory::makeForm<Quote>();
}
