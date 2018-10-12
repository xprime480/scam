
#include "util/ArgListHelper.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern ExprHandle argsToNumeric(ExprHandle const & args,
                                    vector<double> & ns,
                                    string const & context);

    extern ExprHandle argsToStrings(ExprHandle const & args,
                                    vector<double> & ns,
                                    string const & context);

    extern ExprHandle makeNumeric(ExprHandle const & state, double value);

    extern ExprHandle compareNumeric(ExprHandle const & args,
                                     string const & context,
                                     shared_ptr<OpImpl> impl);

    extern ExprHandle compareStrings(ExprHandle const & args,
                                     string const & context,
                                     shared_ptr<OpImpl> impl);
}

ExprHandle scam::numericAlgorithm(ExprHandle const & args,
                                  string const & context,
                                  NumericalAlgorithm algo)
{
    vector<double> ns;

    ExprHandle state = argsToNumeric(args, ns, context);
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
    cerr << "start of compareAlgorithm\n";

    ExprHandle rv;

    rv = compareNumeric(args, context, impl);
    if ( rv->isBoolean() ) {
        cerr << "returning result of numeric\n";
        return rv;
    }

    rv = compareStrings(args, context, impl);
    if ( rv->isBoolean() ) {
        cerr << "returning result of string\n";
        return rv;
    }

    cerr << "returning error\n";
    stringstream s;
    s << "Invalid arguments to comparison: " << context
      << " " << args->toString();
    return ExpressionFactory::makeError(s.str());
}

namespace
{
    bool argToNumeric(ExprHandle const & arg,
                      vector<double> & ns,
                      string const & context,
                      ExprHandle & rv)
    {
        if ( ! arg->isNumeric() ) {
            stringstream s;
            s << context << " expects numeric, got " << arg->toString();
            rv = ExpressionFactory::makeError(s.str());
            return false;
        }

        if ( rv->truth() && ! arg->isInteger() ) {
            rv = ExpressionFactory::makeBoolean(false);
        }

        ns.push_back(arg->toFloat());
        return true;
    }

    bool argToStrings(ExprHandle const & arg,
                      vector<string> & ns,
                      string const & context,
                      ExprHandle & rv)
    {
        if ( ! arg->isString() ) {
            stringstream s;
            s << context << " expects string, got " << arg->toString();
            rv = ExpressionFactory::makeError(s.str());
            return false;
        }

        ns.push_back(arg->toString());
        return true;
    }

    ExprHandle argsToNumericList(ExprHandle const & args,
                                 vector<double> & ns,
                                 string const & context)
    {
        ExprHandle rv = ExpressionFactory::makeBoolean(true);

        const size_t len = args->length();
        for ( size_t idx = 0u ; idx < len ; ++idx ) {
            ExprHandle const arg = args->nth(idx);
            if ( ! argToNumeric(arg, ns, context, rv) ) {
                break;
            }
        }

        return rv;
    }

    ExprHandle argsToNumeric(ExprHandle const & args,
                             vector<double> & ns,
                             string const & context)
    {
        ExprHandle rv = ExpressionFactory::makeBoolean(true);

        if ( args->isList() ) {
            rv = argsToNumericList(args, ns, context);
        }
        else {
            stringstream s;
            s << context << " expects list, got " << args->toString();
            rv = ExpressionFactory::makeError(s.str());
        }

        return rv;
    }

    ExprHandle argsToStringsList(ExprHandle const & args,
                                 vector<string> & ns,
                                 string const & context)
    {
        ExprHandle rv = ExpressionFactory::makeBoolean(true);

        const size_t len = args->length();
        for ( size_t idx = 0u ; idx < len ; ++idx ) {
            ExprHandle const arg = args->nth(idx);
            if ( ! argToStrings(arg, ns, context, rv) ) {
                break;
            }
        }

        return rv;
    }

    ExprHandle argsToStrings(ExprHandle const & args,
                             vector<string> & ns,
                             string const & context)
    {
        ExprHandle rv = ExpressionFactory::makeBoolean(true);

        if ( args->isList() ) {
            rv = argsToStringsList(args, ns, context);
        }
        else {
            stringstream s;
            s << context << " expects list, got " << args->toString();
            rv = ExpressionFactory::makeError(s.str());
        }

        return rv;
    }

    ExprHandle makeNumeric(ExprHandle const & state, double value)
    {
        if ( state->truth() ) {
            return ExpressionFactory::makeInteger((int)value);
        }
        return ExpressionFactory::makeFloat(value);
    }

    ExprHandle compareNumeric(ExprHandle const & args,
                              string const & context,
                              shared_ptr<OpImpl> impl)
    {
        ExprHandle rv;

        vector<double> ns;
        rv = argsToNumeric(args, ns, context);
        cerr << "compareNumeric conversion: " << rv->toString() << "\n";

        if ( rv->isBoolean() ) {
            bool answer = impl->apply(ns);
            rv = ExpressionFactory::makeBoolean(answer);
        }

        return rv;
    }

    ExprHandle compareStrings(ExprHandle const & args,
                              string const & context,
                              shared_ptr<OpImpl> impl)
    {
        ExprHandle rv;

        vector<string> ns;
        rv = argsToStrings(args, ns, context);
        cerr << "compareStrings conversion: " << rv->toString() << "\n";

        if ( rv->isBoolean() ) {
            bool answer = impl->apply(ns);
            rv = ExpressionFactory::makeBoolean(answer);
        }

        return rv;
    }
}
