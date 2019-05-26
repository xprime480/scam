#include "prim/MathOps.hpp"

#include "Continuation.hpp"
#include "expr/ValueFactory.hpp"
#include "input/NumericListParser.hpp"

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

void MathOp::applyArgs(ScamValue args, Continuation * cont)
{
    string const context = writeValue(this);
    NumericListParser * parser =
        standardMemoryManager.make<NumericListParser>();

    if ( ! parser->accept(args) ) {
        failedArgParseMessage(context.c_str(), "(num*)", args, cont);
    }
    else {
        ScamValue rv = numericAlgorithm(parser, context, algo);
        cont->run(rv);
    }
}

namespace
{
    ExtendedNumeric
    do_add(vector<ExtendedNumeric> const & ns, ScamValue & state)
    {
        ExtendedNumeric zero(makeInteger(0, true));
        return accumulate(ns.begin(), ns.end(), zero);
    }

    ExtendedNumeric
    do_sub(vector<ExtendedNumeric> const & ns, ScamValue & state)
    {
        ExtendedNumeric zero(makeInteger(0, true));
        ExtendedNumeric total = zero;

        switch ( ns.size() ) {
        case 0:
            break;

        case 1:
            total = - ns[0];
            break;

        default:
            total = ns[0] - accumulate(++(ns.begin()), ns.end(), zero);
            break;
        }
        return total;
    }

    ExtendedNumeric
    do_mul(vector<ExtendedNumeric> const & ns, ScamValue & state)
    {
        ExtendedNumeric one(makeInteger(1, true));
        return accumulate(ns.begin(), ns.end(),
                          one,
                          multiplies<ExtendedNumeric>());
    }

    ExtendedNumeric
    do_div(vector<ExtendedNumeric> const & ns, ScamValue & state)
    {
        ExtendedNumeric zero(makeInteger(0, true));
        ExtendedNumeric one(makeInteger(1, true));

        state = makeBoolean(false);
        if ( ns.empty() ) {
            return one;
        }
        else {
            auto iter = ns.begin();
            if ( ns.size() > 1 ) {
                iter++;
            }
            if ( ns.end() != find(iter, ns.end(), zero) ) {
                state = makeError("Division By Zero");
                return zero;
            }
        }

        ExtendedNumeric total = one;
        switch ( ns.size() ) {
        case 0:
            break;

        case 1:
            total = total / ns[0];
            break;

        default:
            total = ns[0];
            total = total / accumulate(++(ns.begin()), ns.end(),
                                       one,
                                       multiplies<ExtendedNumeric>());
            break;
        }
        return total;
    }

    ExtendedNumeric
    do_mod(vector<ExtendedNumeric> const & ns, ScamValue & state)
    {
        ExtendedNumeric zero(makeInteger(0, true));

        state = makeBoolean(false);
        if ( ns.size() < 2 ) {
            return zero;
        }
        if ( zero == ns[1] ) {
            state = makeError("Modulus By Zero");
            return zero;
        }

        return ns[0] % ns[1];
    }
}

#define MATH_OP_DEFINE(Name, Proc) \
    static const MathOpDef Name ## def { #Name, Proc }; \
                                                         \
    Name::Name()                                         \
        : MathOp(Name ## def) {}                         \
    Name * Name::makeInstance() { return new Name(); }

MATH_OP_DEFINE(Add, do_add);
MATH_OP_DEFINE(Sub, do_sub);
MATH_OP_DEFINE(Mul, do_mul);
MATH_OP_DEFINE(Div, do_div);
MATH_OP_DEFINE(Mod, do_mod);

#undef MATH_OP_DEFINE
