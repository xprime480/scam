
#include "prim/Add.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "util/ArgListHelper.hpp"

#include <numeric>
#include <sstream>

using namespace scam;
using namespace std;

Add::Add()
    : Primitive("Add")
{
}

void Add::applyArgs(ExprHandle const & args, ContHandle cont)
{
    string const context = toString();
    NumericalAlgorithm algo = [] ( vector<double> const & ns ) -> double {
        return accumulate(ns.begin(), ns.end(), 0.0);
    };
    ExprHandle rv = numericAlgorithm(args, context, algo);
    cont->run(rv);
}

ExprHandle Add::clone()
{
    return ExpressionFactory::makeForm<Add>();
}
