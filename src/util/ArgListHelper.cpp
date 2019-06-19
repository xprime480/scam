#include "util/ArgListHelper.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "ScamException.hpp"
#include "expr/ExtendedNumeric.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "input/NumericListParser.hpp"
#include "input/RelopsListParser.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern ScamValue statusToError(const char * name, ScamValue status);
}

ArgListHelper::ArgListHelper(ScamValue args)
    : args(args)
    , lastRead(0)
    , status(makeNothing())
{
    if ( ! isList(args)  ) {
        status = makeError("Expected list <%{1}>", args);
    }
}

ScamValue ArgListHelper::getStatus()
{
    return status;
}

ScamValue ArgListHelper::peek()
{
    ScamValue rv = makeNothing();

    if ( lastRead < length(args) ) {
        rv = nthcar(args, lastRead);
    }

    return rv;
}

ScamValue ArgListHelper::getAnyValue(ScamValue & value)
{
    value = makeNothing();

    if ( lastRead < length(args) ) {
        value = nthcar(args, lastRead);
        ++lastRead;
    }
    else {
        status = makeError("parameter %{0} missing",
                           makeInteger(1+lastRead, true));
    }

    return status;
}

ScamValue ArgListHelper::getCharacter(char & c)
{
    return getOneArg<char>(c, isChar, asChar, "character");
}

ScamValue ArgListHelper::getInteger(int & value)
{
    return getOneArg<int>(value, isInteger, asInteger, "integer");
}

ScamValue ArgListHelper::getNonNegativeInteger(int & value)
{
    auto pred = [] (ScamValue value) -> bool
    {
        return isInteger(value) && (asInteger(value) >= 0);
    };
    return getOneArg<int>(value, pred, asInteger, "non-negative integer");
}

ScamValue ArgListHelper::getString(string & value)
{
    return getOneArg<string>(value, isString, asString, "string");
}

ScamValue ArgListHelper::getError(ScamValue & value)
{
    return getOneArg<ScamValue>(value, isError, identity, "error-object");
}

ScamValue ArgListHelper::getPair(ScamValue & value)
{
    return getOneArg<ScamValue>(value, isPair, identity, "pair");
}

ScamValue ArgListHelper::getApplicable(ScamValue & value)
{
    return getOneArg<ScamValue>(value, isApplicable, identity, "procedure");
}

ScamValue ArgListHelper::getIndex(int & index, int refParameter)
{
    ScamValue refObj = nthcar(args, refParameter);
    int refLen = length(refObj);

    auto pred = [refLen] (ScamValue value) -> bool
    {
        if ( ! isInteger(value) ) {
            return false;
        }
        int idx = asInteger(value);
        return ( (idx >= 0) && (idx < refLen) );
    };

    stringstream s;
    s << "index into parameter "
      << (1 + refParameter)
      << " ('"
      << writeValue(refObj)
      << "')";

    return getOneArg<int>(index, pred, asInteger, s.str().c_str());
}

ScamValue ArgListHelper::getZeroPlus(ScamValue & value, ValuePredicate pred)
{
    return getCount(value, pred, 0, 9999);
}

ScamValue ArgListHelper::getCount(ScamValue & value,
                                  ValuePredicate pred,
                                  int min,
                                  int max)
{
    vector<ScamValue> values;

    while ( (int)values.size() < max ) {
        if ( lastRead == length(args) ) {
            break;
        }

        ScamValue peek = nthcar(args, lastRead);
        if ( pred(peek) ) {
            values.push_back(peek);
            ++lastRead;
        }
        else {
            break;
        }
    }

    if ( (int)values.size() < min ) {
        status = makeError("Too Few Values (%{1} of at least %{0})",
                           makeInteger(min, true),
                           makeInteger(values.size(), true));
        lastRead -= values.size();
    }
    else {
        value = makeList(values);
    }

    return status;
}

ScamValue ArgListHelper::getOptional(ScamValue & value, ValuePredicate pred)
{
    value = makeNothing();

    if ( lastRead != length(args) ) {
        ScamValue peek = nthcar(args, lastRead);
        if ( pred(peek) ) {
            value = peek;
            ++lastRead;
        }
    }

    return status;
}

ScamValue ArgListHelper::getSublistOf(ScamValue & value, ValuePredicate pred)
{
    ScamValue temp;
    getAnyValue(temp);

    if ( isNothing(status) ) {
        if ( isList(temp) ) {
            const int len = length(temp);
            for ( int idx = 0 ; idx < len ; ++idx ) {
                ScamValue val = nthcar(temp, idx);
                if ( ! pred(val) ) {
                    return makeError("invalid value at position %{1} (%{0})",
                                     val,
                                     makeInteger(idx+1, true));
                }
            }
            value = temp;
        }
        else {
            status = makeError("expected list (%{0})", temp);
        }
    }

    return status;
}

ScamValue ArgListHelper::finish()
{
    if ( lastRead != length(args) ) {
        status = makeError("Expected %{0} values, got %{1}",
                           makeInteger(lastRead, true),
                           makeInteger(length(args), true));
    }

    return status;
}

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

    template <typename T, typename ParserType>
    ScamValue argsToType(ParserType * parser,
                          vector<T> & ns,
                          string const & context)
    {
        ScamValue rv = makeBoolean(true);

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
        if ( isBoolean(rv) ) {
            bool answer = impl->apply(ns);
            rv = makeBoolean(answer);
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
    if ( isError(state) ) {
        return state;
    }

    ExtendedNumeric total = algo(ns, state);
    if ( isError(state) ) {
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
                                 Continuation * cont,
                                 ScamEngine * engine)
{
    ScamValue err = makeError("%{0} expected %{1}; got \"%{2}\"",
                              makeSymbol(who),
                              makeString(exp),
                              act);
    engine->handleError(err);
}


/* * * * * * NEW WORLD CODE * * * * */


size_t scam::length(const string & v)
{
    return v.size();
}

namespace
{
    template <typename F>
    bool wantOneValue(const char * name,
                      Continuation * cont,
                      ScamEngine * engine,
                      F getter)
    {
        ScamValue status = getter();
        if ( isNothing(status) ) {
            return true;
        }

        ScamValue err = statusToError(name, status);
        engine->handleError(err);
        return false;
    }
}

bool scam::wantObject(const char * name,
                      ArgListHelper & helper,
                      Continuation * cont,
                      ScamEngine * engine,
                      ScamValue & value)
{
    auto func = [&] () -> ScamValue
    {
        return helper.getAnyValue(value);
    };
    return wantOneValue(name, cont, engine, func);
}

bool scam::wantChar(const char * name,
                    ArgListHelper & helper,
                    Continuation * cont,
                    ScamEngine * engine,
                    char & c)
{
    auto func = [&] () -> ScamValue
    {
        return helper.getCharacter(c);
    };
    return wantOneValue(name, cont, engine, func);
}

bool scam::wantNonNegativeInteger(const char * name,
                                  ArgListHelper & helper,
                                  Continuation * cont,
                                  ScamEngine * engine,
                                  int & count)
{
    auto func = [&] () -> ScamValue
    {
        return helper.getNonNegativeInteger(count);
    };
    return wantOneValue(name, cont, engine, func);
}

bool scam::wantString(const char * name,
                      ArgListHelper & helper,
                      Continuation * cont,
                      ScamEngine * engine,
                      string & str)
{
    auto func = [&] () -> ScamValue
    {
        return helper.getString(str);
    };
    return wantOneValue(name, cont, engine, func);
}

bool scam::wantMutableString(const char * name,
                             ArgListHelper & helper,
                             Continuation * cont,
                             ScamEngine * engine,
                             ScamValue & value)
{
    auto func = [&] () -> ScamValue
    {
        ScamValue temp = helper.peek();
        string str;
        ScamValue status = helper.getString(str);
        if ( ! isNothing(status) ) {
            return status;
        }

        if ( isImmutable(temp) ) {
            ScamValue err =
                makeError("Cannot mutate constant string %{0}", temp);
            return err;
        }

        value = temp;
        return makeNothing();
    };

    return wantOneValue(name, cont, engine, func);
}

bool scam::wantError(const char * name,
                     ArgListHelper & helper,
                     Continuation * cont,
                     ScamEngine * engine,
                     ScamValue & error)
{
    auto func = [&] () -> ScamValue
    {
        return helper.getError(error);
    };
    return wantOneValue(name, cont, engine, func);
}

bool scam::wantPair(const char * name,
                    ArgListHelper & helper,
                    Continuation * cont,
                    ScamEngine * engine,
                    ScamValue & value)
{
    auto func = [&] () -> ScamValue
    {
        return helper.getPair(value);
    };
    return wantOneValue(name, cont, engine, func);
}

bool scam::wantMutablePair(const char * name,
                           ArgListHelper & helper,
                           Continuation * cont,
                           ScamEngine * engine,
                           ScamValue & value)
{
    auto func = [&] () -> ScamValue
    {
        ScamValue temp;
        ScamValue status = helper.getPair(temp);
        if ( ! isNothing(status) ) {
            return status;
        }

        if ( isImmutable(temp) ) {
            ScamValue err = makeError("Cannot mutate constant pair %{0}", temp);
            return err;
        }

        value = temp;
        return makeNothing();
    };

    return wantOneValue(name, cont, engine, func);
}

bool scam::wantApplicable(const char * name,
                          ArgListHelper & helper,
                          Continuation * cont,
                          ScamEngine * engine,
                          ScamValue & value,
                          int paramCount)
{
    auto func = [&] () -> ScamValue
    {
        // figure out how to get parameter count
        return helper.getApplicable(value);
    };
    return wantOneValue(name, cont, engine, func);
}

bool scam::wantIndex(const char * name,
                     ArgListHelper & helper,
                     Continuation * cont,
                     ScamEngine * engine,
                     int & index,
                     int ref)
{
    auto func = [&] () -> ScamValue
    {
        return helper.getIndex(index, ref);
    };
    return wantOneValue(name, cont, engine, func);
}

bool scam::wantZeroPlus(const char * name,
                        ArgListHelper & helper,
                        Continuation * cont,
                        ScamEngine * engine,
                        ScamValue & value,
                        ValuePredicate pred)
{
    ScamValue status = helper.getZeroPlus(value, pred);

    if ( isNothing(status) ) {
        return true;
    }

    ScamValue err = statusToError(name, status);
    engine->handleError(err);
    return false;
}

bool scam::wantCount(const char * name,
                     ArgListHelper & helper,
                     Continuation * cont,
                     ScamEngine * engine,
                     ScamValue & value,
                     ValuePredicate pred,
                     int min,
                     int max)
{
    ScamValue status = helper.getCount(value, pred, min, max);

    if ( isNothing(status) ) {
        return true;
    }

    ScamValue err = statusToError(name, status);
    engine->handleError(err);
    return false;
}

bool scam::wantSublistOf(const char * name,
                         ArgListHelper & helper,
                         Continuation * cont,
                         ScamEngine * engine,
                         ScamValue & value,
                         ValuePredicate pred)
{
    ScamValue status = helper.getSublistOf(value, pred);

    if ( isNothing(status) ) {
        return true;
    }

    ScamValue err = statusToError(name, status);
    engine->handleError(err);
    return false;
}

bool scam::finishArgs(const char * name,
                      ArgListHelper & helper,
                      Continuation * cont,
                      ScamEngine * engine)
{
    ScamValue status = helper.finish();
    if ( isNothing(status) ) {
        return true;
    }

    ScamValue err = statusToError(name, status);
    engine->handleError(err);
    return false;
}

bool scam::getTwoObjs(ScamValue args,
                      Continuation * cont,
                      ScamEngine * engine,
                      const char * name,
                      ScamValue & obj1,
                      ScamValue & obj2)
{
    ArgListHelper helper(args);

    if ( ! wantObject(name, helper, cont, engine, obj1) ) {
        return false;
    }
    if ( ! wantObject(name, helper, cont, engine, obj2) ) {
        return false;
    }
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return false;
    }

    return true;
}

ScamValue scam::identity(ScamValue value)
{
    return value;
}

namespace
{
    ScamValue statusToError(const char * name, ScamValue status)
    {
        stringstream s;
        s << name << ": ";

        ScamValue rv;

        if ( isError(status) ) {
            s << status->errorMessage();
            rv = makeError(s.str().c_str(), status->errorIrritants());
        }
        else {
            s << writeValue(status);
            rv = makeError(s.str().c_str());
        }

        return rv;
    }
}
