
#include "prim/MathOps.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <algorithm>
#include <numeric>
#include <sstream>

using namespace scam;
using namespace std;

MathOp::MathOp(MathOpDef const & def)
    : Primitive(def.name)
    , algo(def.algo)
{
}

void MathOp::applyArgs(ScamExpr * args, ContHandle cont)
{
    string const context = toString();
    ExprHandle rv = numericAlgorithm(args, context, algo);
    cont->run(rv.get());
}

namespace
{
    double do_add(vector<double> const & ns, ExprHandle & state)
    {
        return accumulate(ns.begin(), ns.end(), 0.0);
    }

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

    double do_mul(vector<double> const & ns, ExprHandle & state)
    {
        return accumulate(ns.begin(), ns.end(), 1.0,
                          multiplies<double>());
    }

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

#define MATH_OP_DEFINE(Name, Proc) \
    static const MathOpDef Name ## def { #Name, Proc }; \
                                                         \
    Name::Name()                                         \
        : MathOp(Name ## def) {}

MATH_OP_DEFINE(Add, do_add);
MATH_OP_DEFINE(Sub, do_sub);
MATH_OP_DEFINE(Mul, do_mul);
MATH_OP_DEFINE(Div, do_div);

#undef MATH_OP_DEFINE
