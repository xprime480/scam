#include "util/ArgListHelper.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "expr/ExtendedNumeric.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    template <typename T>
    struct TypeChecks
    {
        static bool isType(ScamValue arg) { return false; }
        static bool isSubType(ScamValue arg) { return false; }
        static T convert(ScamValue arg) { return T(); }
        static string id() { return ""; }
    };

    template <>
    struct TypeChecks<double>
    {
        static bool isType(ScamValue arg)
        {
            return isNumeric(arg);
        }

        static bool isSubType(ScamValue arg)
        {
            return isInteger(arg);
        }

        static double convert(ScamValue arg)
        {
            return asDouble(arg);
        }

        static string id() {
            return "numeric";
        }
    };

    template <>
    struct TypeChecks<string>
    {
        static bool isType(ScamValue arg)
        {
            return isString(arg);
        }

        static bool isSubType(ScamValue arg)
        {
            return isString(arg);
        }

        static string convert(ScamValue arg)
        {
            return writeValue(arg);
        }

        static string id() {
            return "string";
        }
    };

    template <>
    struct TypeChecks<ExtendedNumeric>
    {
        static bool isType(ScamValue arg)
        {
            return isNumeric(arg);
        }

        static bool isSubType(ScamValue arg)
        {
            return false;
        }

        static ExtendedNumeric convert(ScamValue arg)
        {
            ExtendedNumeric tmp(arg);
            return tmp;
        }

        static string id() {
            return "extended numeric";
        }
    };

    template <typename T>
    bool argToType(ScamValue arg,
                   vector<T> & ns,
                   string const & context,
                   ScamValue & rv)
    {
        typedef TypeChecks<T> Checker;

        if ( ! Checker::isType(arg) ) {
            rv = makeError("%{0} expects %{1} (%2)",
                           makeSymbol(context),
                           makeSymbol(Checker::id()),
                           arg);
            return false;
        }

        if ( truth(rv) && ! Checker::isSubType(arg) ) {
            rv = makeBoolean(false);
        }

        ns.push_back(Checker::convert(arg));
        return true;
    }

    template <typename T>
    ScamValue argsToType(ScamValue value,
                         vector<T> & ns,
                         string const & context)
    {
        ScamValue rv = makeBoolean(true);

        const size_t len = length(value);
        for ( size_t idx = 0u ; idx < len ; ++idx ) {
            ScamValue arg = nthcar(value, idx);
            if ( ! argToType(arg, ns, context, rv) ) {
                break;
            }
        }

        return rv;
    }

    template <typename T>
    ScamValue compareType(ScamValue value,
                          string const & context,
                          shared_ptr<OpImpl> impl)
    {
        ScamValue rv;

        vector<T> ns;
        rv = argsToType(value, ns, context);
        if ( isBoolean(rv) ) {
            bool answer = impl->apply(ns);
            rv = makeBoolean(answer);
        }

        return rv;
    }
}

ScamValue scam::numericAlgorithm(ScamValue value,
                                 string const & context,
                                 NumericalAlgorithm algo)
{
    vector<ExtendedNumeric> ns;

    while ( ! isNull(value) ) {
        ExtendedNumeric en(getCar(value));
        ns.push_back(en);
        value = getCdr(value);
    }

    ScamValue state = makeBoolean(true);
    ExtendedNumeric total = algo(ns, state);
    if ( isError(state) ) {
        return state;
    }

    return total.get();
}

ScamValue scam::compareAlgorithm(ScamValue value,
                                 string const & context,
                                 shared_ptr<OpImpl> impl)
{
    if ( isNull(value) || isNumeric(nthcar(value, 0)) ) {
        return compareType<ExtendedNumeric>(value, context, impl);
    }
    else {
        return compareType<string>(value, context, impl);
    }
}
