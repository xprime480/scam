
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

namespace
{
    double do_sub(vector<double> const & ns, ExprHandle & state)
    {
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
    }
}

void Sub::applyArgs(ExprHandle const & args, ContHandle cont)
{
    string const context = toString();
    ExprHandle rv = numericAlgorithm(args, context, do_sub);
    cont->run(rv);
}
