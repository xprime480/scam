
#include "util/ArgListHelper.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>
#include <sstream>

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
                   ExprHandle & rv)
    {
        typedef TypeChecks<T> Checker;

        if ( ! Checker::isType(arg) ) {
            stringstream s;
            s << context << " expects " << Checker::id()
              << ", got " << arg->toString();
            rv = ExpressionFactory::makeError(s.str());
            return false;
        }

        if ( rv->truth() && ! Checker::isSubType(arg) ) {
            rv = ExpressionFactory::makeBoolean(false);
        }

        ns.push_back(Checker::convert(arg));
        return true;
    }

    template <typename T>
    ExprHandle argsToType(ScamExpr * args,
                          vector<T> & ns,
                          string const & context)
    {
        ExprHandle rv = ExpressionFactory::makeBoolean(true);

        const size_t len = args->length();
        for ( size_t idx = 0u ; idx < len ; ++idx ) {
            ExprHandle arg = args->nthcar(idx);
            if ( ! argToType(arg.get(), ns, context, rv) ) {
                break;
            }
        }

        return rv;
    }

    extern ExprHandle makeNumeric(ExprHandle & state, double value);

    template <typename T>
    ExprHandle compareType(ScamExpr * args,
                           string const & context,
                           shared_ptr<OpImpl> impl)
    {
        ExprHandle rv;

        vector<T> ns;
        rv = argsToType(args, ns, context);
        if ( rv->isBoolean() ) {
            bool answer = impl->apply(ns);
            rv = ExpressionFactory::makeBoolean(answer);
        }

        return rv;
    }
}

ExprHandle scam::numericAlgorithm(ScamExpr * args,
                                  string const & context,
                                  NumericalAlgorithm algo)
{
    vector<double> ns;

    ExprHandle state = argsToType(args, ns, context);
    if ( state->error() ) {
        return state;
    }

    double total = algo(ns, state);
    if ( state->error() ) {
        return state;
    }

    return makeNumeric(state, total);
}

ExprHandle scam::compareAlgorithm(ScamExpr * args,
                                  string const & context,
                                  shared_ptr<OpImpl> impl)
{
    ExprHandle rv;

    rv = compareType<double>(args, context, impl);
    if ( rv->isBoolean() ) {
        return rv;
    }

    rv = compareType<string>(args, context, impl);
    if ( rv->isBoolean() ) {
        return rv;
    }

    stringstream s;
    s << "Invalid arguments to comparison: " << context
      << " " << args->toString();
    return ExpressionFactory::makeError(s.str());
}

namespace
{
    ExprHandle makeNumeric(ExprHandle & state, double value)
    {
        if ( state->truth() ) {
            return ExpressionFactory::makeInteger((int)value);
        }
        return ExpressionFactory::makeFloat(value);
    }
}
