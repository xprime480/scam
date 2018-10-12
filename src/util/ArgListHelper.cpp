
#include "util/ArgListHelper.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern ExprHandle argsToNumeric(ExprHandle const & args,
                                    vector<double> & ns,
                                    string const & context);

    extern ExprHandle makeNumeric(ExprHandle const & state, double value);
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
    vector<double> x;
    bool rv = impl->apply(x);
    return ExpressionFactory::makeBoolean(rv);
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
            s << context << " expects list of numeric, got " << args->toString();
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
}
