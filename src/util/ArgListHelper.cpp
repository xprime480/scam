#include "util/ArgListHelper.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"
#include "expr/ExtendedNumeric.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "input/NumericListParser.hpp"
#include "input/RelopsListParser.hpp"

using namespace scam;
using namespace std;

namespace
{
}

ArgListHelper::ArgListHelper(ScamValue args)
    : args(args)
    , lastRead(0)
    , status(makeNothing())
{
    if ( ! isList(args)  ) {
        status = makeErrorExtended("ArgListHelper expected list, got '",
                                   writeValue(args),
                                   "'");
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
        status = makeErrorExtended("parameter ", (1+lastRead), " missing");
    }

    return status;
}

ScamValue ArgListHelper::getCharacter(char & c)
{
    ScamValue temp;
    getAnyValue(temp);

    if ( isNothing(status) ) {
        if ( isChar(temp) ) {
            c = asChar(temp);
        }
        else {
            status = makeErrorExtended("expected character for parameter ",
                                       lastRead,
                                       " got '",
                                       writeValue(temp),
                                       "'");
        }
    }

    return status;
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

ScamValue ArgListHelper::getPair(ScamValue & value)
{
    return getOneArg<ScamValue>(value, isPair, identity, "pair");
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
        status = makeErrorExtended("found ",
                                   values.size(),
                                   " values, wanted at least ",
                                   min);
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
                    return makeErrorExtended("list for parameter ",
                                             lastRead,
                                             " contains invalid value '",
                                             writeValue(val),
                                             "'");
                }
            }
            value = temp;
        }
        else {
            status = makeErrorExtended("expected list for parameter ",
                                       lastRead,
                                       " got '",
                                       writeValue(temp),
                                       "'");
        }
    }

    return status;
}

ScamValue ArgListHelper::finish()
{
    if ( lastRead != length(args) ) {
        status = makeErrorExtended(lastRead,
                                   " parameter needed, ",
                                   length(args),
                                   " given");
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
            rv = makeErrorExtended(context,
                                   " expects ",
                                   Checker::id(),
                                   ", got ",
                                   writeValue(arg));
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
                                 Continuation * cont)
{
    ScamValue err = makeErrorExtended(who,
                                      " expected \"",
                                      exp,
                                      "\"; got \"",
                                      writeValue(act),
                                      "\"");
    cont->run(err);
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
                      F getter)
    {
        ScamValue status = getter();
        if ( isNothing(status) ) {
            return true;
        }

        ScamValue err = makeErrorExtended(name, ": ", writeValue(status));
        cont->run(err);
        return false;
    }
}

bool scam::wantObject(const char * name,
                      ArgListHelper & helper,
                      Continuation * cont,
                      ScamValue & value)
{
    auto func = [&] () -> ScamValue
    {
        return helper.getAnyValue(value);
    };
    return wantOneValue(name, cont, func);
}

bool scam::wantChar(const char * name,
                    ArgListHelper & helper,
                    Continuation * cont,
                    char & c)
{
    auto func = [&] () -> ScamValue
    {
        return helper.getCharacter(c);
    };
    return wantOneValue(name, cont, func);
}

bool scam::wantNonNegativeInteger(const char * name,
                                  ArgListHelper & helper,
                                  Continuation * cont,
                                  int & count)
{
    auto func = [&] () -> ScamValue
    {
        return helper.getNonNegativeInteger(count);
    };
    return wantOneValue(name, cont, func);
}

bool scam::wantString(const char * name,
                      ArgListHelper & helper,
                      Continuation * cont,
                      string & str)
{
    auto func = [&] () -> ScamValue
    {
        return helper.getString(str);
    };
    return wantOneValue(name, cont, func);
}

bool scam::wantMutableString(const char * name,
                             ArgListHelper & helper,
                             Continuation * cont,
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
                makeErrorExtended("Cannot mutate constant string ",
                                  writeValue(temp));
            return err;
        }

        value = temp;
        return makeNothing();
    };

    return wantOneValue(name, cont, func);
}

bool scam::wantPair(const char * name,
                    ArgListHelper & helper,
                    Continuation * cont,
                    ScamValue & value)
{
    auto func = [&] () -> ScamValue
    {
        return helper.getPair(value);
    };
    return wantOneValue(name, cont, func);
}

bool scam::wantMutablePair(const char * name,
                           ArgListHelper & helper,
                           Continuation * cont,
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
            ScamValue err =
                makeErrorExtended("Cannot mutate constant pair ",
                                  writeValue(temp));
            return err;
        }

        value = temp;
        return makeNothing();
    };

    return wantOneValue(name, cont, func);
}

bool scam::wantIndex(const char * name,
                     ArgListHelper & helper,
                     Continuation * cont,
                     int & index,
                     int ref)
{
    auto func = [&] () -> ScamValue
    {
        return helper.getIndex(index, ref);
    };
    return wantOneValue(name, cont, func);
}

bool scam::wantZeroPlus(const char * name,
                        ArgListHelper & helper,
                        Continuation * cont,
                        ScamValue & value,
                        ValuePredicate pred)
{
    ScamValue status = helper.getZeroPlus(value, pred);

    if ( isNothing(status) ) {
        return true;
    }

    ScamValue err = makeErrorExtended(name, ": ", writeValue(status));
    cont->run(err);
    return false;
}

bool scam::wantCount(const char * name,
                     ArgListHelper & helper,
                     Continuation * cont,
                     ScamValue & value,
                     ValuePredicate pred,
                     int min,
                     int max)
{
    ScamValue status = helper.getCount(value, pred, min, max);

    if ( isNothing(status) ) {
        return true;
    }

    ScamValue err = makeErrorExtended(name, ": ", writeValue(status));
    cont->run(err);
    return false;
}

bool scam::wantSublistOf(const char * name,
                         ArgListHelper & helper,
                         Continuation * cont,
                         ScamValue & value,
                         ValuePredicate pred)
{
    ScamValue status = helper.getSublistOf(value, pred);

    if ( isNothing(status) ) {
        return true;
    }

    ScamValue err = makeErrorExtended(name, ": ", writeValue(status));
    cont->run(err);
    return false;
}

bool scam::finishArgs(const char * name,
                      ArgListHelper & helper,
                      Continuation * cont,
                      const char * msg)
{
    ScamValue status = helper.finish();
    if ( isNothing(status) ) {
        return true;
    }

    ScamValue err =
        makeErrorExtended(name, ": ", (msg ? msg : writeValue(status)));
    cont->run(err);
    return false;
}

bool scam::getTwoObjs(ScamValue args,
                      Continuation * cont,
                      const char * name,
                      ScamValue & obj1,
                      ScamValue & obj2)
{
    ArgListHelper helper(args);

    if ( ! wantObject(name, helper, cont, obj1) ) {
        return false;
    }
    if ( ! wantObject(name, helper, cont, obj2) ) {
        return false;
    }
    if ( ! finishArgs(name, helper, cont) ) {
        return false;
    }

    return true;
}

ScamValue scam::identity(ScamValue value)
{
    return value;
}
