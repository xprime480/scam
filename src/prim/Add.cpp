
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

namespace
{
    double do_add(vector<double> const & ns, ExprHandle & state)
    {
        return accumulate(ns.begin(), ns.end(), 0.0);
    }
}

void Add::applyArgs(ExprHandle const & args, ContHandle cont)
{
    string const context = toString();
    ExprHandle rv = numericAlgorithm(args, context, do_add);
    cont->run(rv);
}

ExprHandle Add::clone()
{
    return ExpressionFactory::makeForm<Add>();
}
