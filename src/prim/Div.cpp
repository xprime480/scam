
#include "prim/Div.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "util/ArgListHelper.hpp"

#include <algorithm>
#include <numeric>

using namespace scam;
using namespace std;

Div::Div()
    : Primitive("Div")
{
}

namespace
{
    double do_div(vector<double> const & ns, ExprHandle & state)
    {
        if ( ns.end() != find(ns.begin(), ns.end(), 0.0) ) {
            state = ExpressionFactory::makeError("Division By Zero");
            return 0.0;
        }
        else {
            state = ExpressionFactory::makeBoolean(false);
        }

        double total { 1.0 };
        switch ( ns.size() ) {
        case 0:
            break;

        case 1:
            total /= ns[0];
            break;

        default:
            total  = ns[0];
            total /= accumulate(++(ns.begin()), ns.end(), 1.0,
                                multiplies<double>());
            break;
        }
        return total;
    }
}

void Div::applyArgs(ExprHandle const & args, ContHandle cont)
{
    string const context = toString();
    ExprHandle rv = numericAlgorithm(args, context, do_div);
    cont->run(rv);
}

ExprHandle Div::clone() const
{
    return ExpressionFactory::makeForm<Div>();
}
