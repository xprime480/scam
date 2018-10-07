
#include "prim/Sub.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "util/ArgListHelper.hpp"

#include <numeric>
#include <sstream>

using namespace scam;
using namespace std;

Sub::Sub()
    : Primitive("Sub")
{
}

void Sub::applyArgs(ExprHandle const & args, ContHandle cont)
{
    string const context = toString();
    NumericalAlgorithm algo = [] ( vector<double> const & ns ) -> double {
        double total { 0 };
        switch ( ns.size() ) {
        case 0:
            break;

        case 1:
            total = - ns[0];
            break;

        default:
            total = ns[0] - accumulate(++(ns.begin()), ns.end(), 0.0);
            break;
        }
        return total;
    };
    ExprHandle rv = numericAlgorithm(args, context, algo);
    cont->run(rv);
}

ExprHandle Sub::clone()
{
    return ExpressionFactory::makeForm<Sub>();
}
