#include "util/ArgListHelper.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
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
            return arg->toReal();
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
            return arg->toString();
        }

        static string id() {
            return "string";
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
                                              arg->toString());
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

    extern ExprHandle makeNumeric(ExprHandle & state, double value);

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
    vector<double> ns;

    ExprHandle state = argsToType(parser, ns, context);
    if ( state->error() ) {
        return state;
    }

    double total = algo(ns, state);
    if ( state->error() ) {
        return state;
    }

    return makeNumeric(state, total);
}

ExprHandle scam::compareAlgorithm(RelopsListParser * parser,
                                  string const & context,
                                  shared_ptr<OpImpl> impl)
{
    if ( parser->isNumeric() ) {
        return compareType<double>(parser, context, impl);
    }
    return compareType<string>(parser, context, impl);
}

namespace
{
    ExprHandle makeNumeric(ExprHandle & state, double value)
    {
        if ( state->truth() ) {
            return ExpressionFactory::makeInteger((int)value, true);
        }
        return ExpressionFactory::makeReal(value, false);
    }
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
                                                  act->toString(),
                                                  "\"");
    cont->run(err);
}
