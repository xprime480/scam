
#include "prim/Mul.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "util/ArgListHelper.hpp"

#include <numeric>
#include <sstream>

using namespace scam;
using namespace std;

Mul::Mul()
    : Primitive("Mul")
{
}

namespace
{
    double do_mul(vector<double> const & ns, ExprHandle & state)
    {
        return accumulate(ns.begin(), ns.end(), 1.0,
                          multiplies<double>());
    }
}

void Mul::applyArgs(ExprHandle const & args, ContHandle cont)
{
    string const context = toString();
    ExprHandle rv = numericAlgorithm(args, context, do_mul);
    cont->run(rv);
}

ExprHandle Mul::clone()
{
    return ExpressionFactory::makeForm<Mul>();
}
