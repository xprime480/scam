#include "util/ArgListHelper.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ExtendedNumeric.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/TypePredicates.hpp"
#include "input/NumericListParser.hpp"
#include "input/RelopsListParser.hpp"

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
            return TypePredicates::isNumeric(arg);
        }

        static bool isSubType(ScamValue arg)
        {
            return TypePredicates::isInteger(arg);
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
            return TypePredicates::isString(arg);
        }

        static bool isSubType(ScamValue arg)
        {
            return TypePredicates::isString(arg);
        }

        static string convert(ScamValue arg)
        {
            return ExprWriter::write(arg);
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
            return TypePredicates::isNumeric(arg);
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
            rv = ExpressionFactory::makeError(context,
                                              " expects ",
                                              Checker::id(),
                                              ", got ",
                                              ExprWriter::write(arg));
            return false;
        }

        if ( TypePredicates::truth(rv) && ! Checker::isSubType(arg) ) {
            rv = ExpressionFactory::makeBoolean(false);
        }

        ns.push_back(Checker::convert(arg));
        return true;
    }

    template <typename T, typename ParserType>
    ScamValue argsToType(ParserType * parser,
                          vector<T> & ns,
                          string const & context)
    {
        ScamValue rv = ExpressionFactory::makeBoolean(true);

        const size_t len = parser->size();
        for ( size_t idx = 0u ; idx < len ; ++idx ) {
            ScamValue arg = parser->get(idx);
            if ( ! argToType(arg, ns, context, rv) ) {
                break;
            }
        }

        return rv;
    }

    template <typename T>
    ScamValue compareType(RelopsListParser * parser,
                           string const & context,
                           shared_ptr<OpImpl> impl)
    {
        ScamValue rv;

        vector<T> ns;
        rv = argsToType(parser, ns, context);
        if ( TypePredicates::isBoolean(rv) ) {
            bool answer = impl->apply(ns);
            rv = ExpressionFactory::makeBoolean(answer);
        }

        return rv;
    }
}

ScamValue scam::numericAlgorithm(NumericListParser * parser,
                                  string const & context,
                                  NumericalAlgorithm algo)
{
    vector<ExtendedNumeric> ns;

    ScamValue state = argsToType(parser, ns, context);
    if ( TypePredicates::error(state) ) {
        return state;
    }

    ExtendedNumeric total = algo(ns, state);
    if ( TypePredicates::error(state) ) {
        return state;
    }

    return total.get();
}

ScamValue scam::compareAlgorithm(RelopsListParser * parser,
                                  string const & context,
                                  shared_ptr<OpImpl> impl)
{
    if ( parser->isNumeric() ) {
        return compareType<ExtendedNumeric>(parser, context, impl);
    }
    return compareType<string>(parser, context, impl);
}

void scam::failedArgParseMessage(const char * who,
                                 const char * exp,
                                 ScamValue act,
                                 Continuation * cont)
{
    ScamValue err = ExpressionFactory::makeError(who,
                                                  " expected \"",
                                                  exp,
                                                  "\"; got \"",
                                                  ExprWriter::write(act),
                                                  "\"");
    cont->run(err);
}
