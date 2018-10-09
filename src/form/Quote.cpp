
#include "form/Quote.hpp"

#include "Continuation.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

Quote::Quote()
    : SpecialForm("quote")
{
}

void Quote::apply(ExprHandle const & args, ContHandle cont, Env & env)
{
    ExprHandle expr = args->getCar();
    cont->run(expr);
}

ExprHandle Quote::clone() const
{
    return ExpressionFactory::makeForm<Quote>();
}
