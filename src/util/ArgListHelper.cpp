#include "util/ArgListHelper.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ExtendedNumeric.hpp"
#include "input/NumericListParser.hpp"
#include "input/RelopsListParser.hpp"

using namespace scam;
using namespace std;

namespace
{
    template <typename T>
    struct TypeChecks
    {
        static bool isType(ExprHandle arg) { return false; }
        static bool isSubType(ExprHandle arg) { return false; }
        static T convert(ExprHandle arg) { return T(); }
        static string id() { return ""; }
    };

    template <>
    struct TypeChecks<double>
    {
        static bool isType(ExprHandle arg)
        {
            return arg->isNumeric();
        }

        static bool isSubType(ExprHandle arg)
        {
            return arg->isInteger();
        }

        static double convert(ExprHandle arg)
        {
            return arg->asDouble();
        }

        static string id() {
            return "numeric";
        }
    };

    template <>
    struct TypeChecks<string>
    {
        static bool isType(ExprHandle arg)
        {
            return arg->isString();
        }

        static bool isSubType(ExprHandle arg)
        {
            return arg->isString();
        }

        static string convert(ExprHandle arg)
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
        static bool isType(ExprHandle arg)
        {
            return arg->isNumeric();
        }

        static bool isSubType(ExprHandle arg)
        {
            return false;
        }

        static ExtendedNumeric convert(ExprHandle arg)
        {
            ExtendedNumeric tmp(arg);
            return tmp;
        }

        static string id() {
            return "extended numeric";
        }
    };

    template <typename T>
    bool argToType(ExprHandle arg,
                   vector<T> & ns,
                   string const & context,
                   ExprHandle & rv)
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

        if ( rv->truth() && ! Checker::isSubType(arg) ) {
            rv = ExpressionFactory::makeBoolean(false);
        }

        ns.push_back(Checker::convert(arg));
        return true;
    }

    template <typename T, typename ParserType>
    ExprHandle argsToType(ParserType * parser,
                          vector<T> & ns,
                          string const & context)
    {
        ExprHandle rv = ExpressionFactory::makeBoolean(true);

        const size_t len = parser->size();
        for ( size_t idx = 0u ; idx < len ; ++idx ) {
            ExprHandle arg = parser->get(idx);
            if ( ! argToType(arg, ns, context, rv) ) {
                break;
            }
        }

        return rv;
    }

    template <typename T>
    ExprHandle compareType(RelopsListParser * parser,
                           string const & context,
                           shared_ptr<OpImpl> impl)
    {
        ExprHandle rv;

        vector<T> ns;
        rv = argsToType(parser, ns, context);
        if ( rv->isBoolean() ) {
            bool answer = impl->apply(ns);
            rv = ExpressionFactory::makeBoolean(answer);
        }

        return rv;
    }
}

ExprHandle scam::numericAlgorithm(NumericListParser * parser,
                                  string const & context,
                                  NumericalAlgorithm algo)
{
    vector<ExtendedNumeric> ns;

    ExprHandle state = argsToType(parser, ns, context);
    if ( state->error() ) {
        return state;
    }

    ExtendedNumeric total = algo(ns, state);
    if ( state->error() ) {
        return state;
    }

    return total.get();
}

ExprHandle scam::compareAlgorithm(RelopsListParser * parser,
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
                                 ExprHandle act,
                                 Continuation * cont)
{
    ExprHandle err = ExpressionFactory::makeError(who,
                                                  " expected \"",
                                                  exp,
                                                  "\"; got \"",
                                                  ExprWriter::write(act),
                                                  "\"");
    cont->run(err);
}
