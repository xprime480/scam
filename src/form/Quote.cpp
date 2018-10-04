
#include "form/Quote.hpp"

#include "ScamContext.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

Quote::Quote()
    : SpecialForm("quote")
{
}

void
Quote::apply(shared_ptr<ScamExpr> const & args, ScamContext const & context)
{
    shared_ptr<ScamExpr> expr = args->getCar();
    context.cont->run(expr);
}

std::shared_ptr<ScamExpr> Quote::clone()
{
    return ExpressionFactory::makeForm<Quote>();
}
