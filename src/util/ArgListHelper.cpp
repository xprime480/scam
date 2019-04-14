#include "util/ArgListHelper.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

namespace
{
    template <typename T>
    struct TypeChecks
    {
        static bool isType(ScamExpr * arg) { return false; }
        static bool isSubType(ScamExpr * arg) { return false; }
        static T convert(ScamExpr * arg) { return T(); }
        static string id() { return ""; }
    };

    template <>
    struct TypeChecks<double>
    {
        static bool isType(ScamExpr * arg)
        {
            return arg->isNumeric();
        }

        static bool isSubType(ScamExpr * arg)
        {
            return arg->isInteger();
        }

        static double convert(ScamExpr * arg)
        {
            return arg->toFloat();
        }

        static string id() {
            return "numeric";
        }
    };

    template <>
    struct TypeChecks<string>
    {
        static bool isType(ScamExpr * arg)
        {
            return arg->isString();
        }

        static bool isSubType(ScamExpr * arg)
        {
            return arg->isString();
        }

        static string convert(ScamExpr * arg)
        {
            return arg->toString();
        }

        static string id() {
            return "string";
        }
    };

    template <typename T>
    bool argToType(ScamExpr * arg,
                   vector<T> & ns,
                   string const & context,
                   ScamExpr * & rv)
    {
        typedef TypeChecks<T> Checker;

        if ( ! Checker::isType(arg) ) {
            rv = ExpressionFactory::makeError(context,
                                              " expects ",
                                              Checker::id(),
                                              ", got ",
                                              arg->toString());
            return false;
        }

        if ( rv->truth() && ! Checker::isSubType(arg) ) {
            rv = ExpressionFactory::makeBoolean(false);
        }

        ns.push_back(Checker::convert(arg));
        return true;
    }

    template <typename T>
    ScamExpr * argsToType(ScamExpr * args,
                          vector<T> & ns,
                          string const & context)
    {
        ScamExpr * rv = ExpressionFactory::makeBoolean(true);

        const size_t len = args->length();
        for ( size_t idx = 0u ; idx < len ; ++idx ) {
            ScamExpr * arg = args->nthcar(idx);
            if ( ! argToType(arg, ns, context, rv) ) {
                break;
            }
        }

        return rv;
    }

    extern ScamExpr * makeNumeric(ScamExpr * & state, double value);

    template <typename T>
    ScamExpr * compareType(ScamExpr * args,
                           string const & context,
                           shared_ptr<OpImpl> impl)
    {
        ScamExpr * rv;

        vector<T> ns;
        rv = argsToType(args, ns, context);
        if ( rv->isBoolean() ) {
            bool answer = impl->apply(ns);
            rv = ExpressionFactory::makeBoolean(answer);
        }

        return rv;
    }
}

ScamExpr * scam::numericAlgorithm(ScamExpr * args,
                                  string const & context,
                                  NumericalAlgorithm algo)
{
    vector<double> ns;

    ScamExpr * state = argsToType(args, ns, context);
    if ( state->error() ) {
        return state;
    }

    double total = algo(ns, state);
    if ( state->error() ) {
        return state;
    }

    return makeNumeric(state, total);
}

ScamExpr * scam::compareAlgorithm(ScamExpr * args,
                                  string const & context,
                                  shared_ptr<OpImpl> impl)
{
    ScamExpr * rv;

    rv = compareType<double>(args, context, impl);
    if ( rv->isBoolean() ) {
        return rv;
    }

    rv = compareType<string>(args, context, impl);
    if ( rv->isBoolean() ) {
        return rv;
    }

    return ExpressionFactory::makeError("Invalid arguments to comparison: ",
                                        context,
                                        " ",
                                        args->toString());
}

namespace
{
    ScamExpr * makeNumeric(ScamExpr * & state, double value)
    {
        if ( state->truth() ) {
            return ExpressionFactory::makeInteger((int)value);
        }
        return ExpressionFactory::makeFloat(value);
    }
}
