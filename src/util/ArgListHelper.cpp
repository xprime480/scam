
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
        static bool isType(ExprHandle const & arg) { return false; }
        static bool isSubType(ExprHandle const & arg) { return false; }
        static T convert(ExprHandle const & arg) { return T(); }
        static string id() { return ""; }
    };

    template <>
    struct TypeChecks<double>
    {
        static bool isType(ExprHandle const & arg)
        {
            return arg->isNumeric();
        }

        static bool isSubType(ExprHandle const & arg)
        {
            return arg->isInteger();
        }

        static double convert(ExprHandle const & arg)
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
        static bool isType(ExprHandle const & arg)
        {
            return arg->isString();
        }

        static bool isSubType(ExprHandle const & arg)
        {
            return arg->isString();
        }

        static string convert(ExprHandle const & arg)
        {
            return arg->toString();
        }

        static string id() {
            return "string";
        }
    };

    template <typename T>
    bool argToType(ExprHandle const & arg,
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
    ExprHandle argsToType(ExprHandle const & args,
                          vector<T> & ns,
                          string const & context)
    {
        ExprHandle rv = ExpressionFactory::makeBoolean(true);

        const size_t len = args->length();
        for ( size_t idx = 0u ; idx < len ; ++idx ) {
            ExprHandle const arg = args->nth(idx);
            if ( ! argToType(arg, ns, context, rv) ) {
                break;
            }
        }

        return rv;
    }

    extern ExprHandle makeNumeric(ExprHandle const & state, double value);

    template <typename T>
    ExprHandle compareType(ExprHandle const & args,
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

ExprHandle scam::numericAlgorithm(ExprHandle const & args,
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

ExprHandle scam::compareAlgorithm(ExprHandle const & args,
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
    ExprHandle makeNumeric(ExprHandle const & state, double value)
    {
        if ( state->truth() ) {
            return ExpressionFactory::makeInteger((int)value);
        }
        return ExpressionFactory::makeFloat(value);
    }
}
