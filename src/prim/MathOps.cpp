
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
        state = ExpressionFactory::makeBoolean(false);
        if ( ns.empty() ) {
            return 1.0;
        }
        else {
            auto iter = ns.begin();
            if ( ns.size() > 1 ) {
                iter++;
            }
            if ( ns.end() != find(iter, ns.end(), 0.0) ) {
                state = ExpressionFactory::makeError("Division By Zero");
                return 0.0;
            }
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

    double do_mod(vector<double> const & ns, ExprHandle & state)
    {
        state = ExpressionFactory::makeBoolean(false);
        if ( ns.size() < 2 ) {
            return 0.0;
        }
        if ( 0 == ns[1] ) {
            state = ExpressionFactory::makeError("Modulus By Zero");
            return 0.0;
        }

        return (double) ((int)ns[0] % (int)ns[1]);
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
MATH_OP_DEFINE(Mod, do_mod);

#undef MATH_OP_DEFINE
