#include "util/Parameter.hpp"

#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "expr/SequenceOps.hpp"
#include "value/ScamData.hpp"
#include "value/ScamToInternal.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    void massageError(const char * prefix, ScamValue original)
    {
        if ( isError(original) ) {
            stringstream s;
            s << prefix << ": " << original->errorMessage();
            original->errorMessage() = s.str();
        }
    }
}

Parameter::Parameter()
    : valid(false)
{
}

Parameter::~Parameter()
{
}

ObjectParameter::ObjectParameter()
    : value(makeNull())
{
}

ScamValue ObjectParameter::transform(ScamValue args)
{
    valid = false;
    ScamValue rv;

    if ( isPair(args) ) {
        valid = true;
        value = getCar(args);
        rv = getCdr(args);
    }
    else {
        rv = makeError("Expecting argument list, got %{0}", args);
        rv->errorCategory() = argsCategory;
    }

    return rv;
}

namespace
{
    ScamValue wrongType(const char * expected, ScamValue value)
    {
        stringstream s;
        s << "Wrong type for parameter, expected " << expected << "; got %{0}";
        ScamValue rv = makeError(s.str().c_str(), value);
        rv->errorCategory() = argsCategory;
        return rv;
    }

    ScamValue
    checker(ScamValue value, ParameterFilter filter, const char * expected)
    {
        ScamValue rv = makeNull();
        if ( ! filter(value) ) {
            rv = wrongType(expected, value);
        }

        return rv;
    }

    ScamValue inexactError(ScamValue value)
    {
        static const char * msg = "count should be exact: %{0}";
        ScamValue err = makeError(msg, value);
        err->errorCategory() = argsCategory;
        return err;
    }


}

ScamValue FilteredParameter::transform(ScamValue args)
{
    valid = false;
    ScamValue rv = ObjectParameter::transform(args);

    if ( ! isUnhandledError(rv) ) {
        ScamValue temp = check();
        if ( isUnhandledError(temp) ) {
            valid = false;
            rv = temp;
        }
    }

    return rv;
}

ScamValue NumericParameter::check()
{
    return checker(value, isNumeric, "number");
}

ScamValue IntegerParameter::check()
{
    return checker(value, isInteger, "integer");
}

ScamValue CharacterParameter::check()
{
    return checker(value, isChar, "character");
}

ScamValue SymbolParameter::check()
{
    return checker(value, isSymbol, "symbol");
}

ScamValue KeywordParameter::check()
{
    return checker(value, isKeyword, "keyword");
}

ScamValue StringParameter::check()
{
    return checker(value, isString, "string");
}

ScamValue ErrorParameter::check()
{
    return checker(value, isError, "error-object");
}

ScamValue PairParameter::check()
{
    return checker(value, isPair, "pair");
}

ScamValue VectorParameter::check()
{
    return checker(value, isVector, "vector");
}

ScamValue DictParameter::check()
{
    return checker(value, isDict, "dictionary");
}

ScamValue ApplicableParameter::check()
{
    return checker(value, isApplicable, "applicable");
}

ScamValue PortParameter::check()
{
    return checker(value, isPort, "port");
}

ScamValue EnvParameter::check()
{
    return checker(value, isEnv, "environment");
}

ScamValue CountParameter::check()
{
    ScamValue rv = checker(value, isInteger, "non-negative integer");
    if ( ! isUnhandledError(rv) ) {
        int count = asInteger(value);

        if ( ! isExact(value) ) {
            rv = inexactError(value);
        }
        else if ( count < 0 ) {
            rv = outOfRange();
        }
    }

    return rv;
}

ScamValue CountParameter::outOfRange()
{
    static const char * msg = "count should be non-negative: %{0}";
    ScamValue err = makeError(msg, value);
    err->errorCategory() = argsCategory;
    return err;
}

IndexLikeParameter::IndexLikeParameter(ObjectParameter & referant)
    : referant(referant)
{
}

ScamValue IndexLikeParameter::transform(ScamValue args)
{
    if ( ! referant.valid ) {
        return invalidReferant();
    }

    ScamValue rv = checkIndexNature(args);
    return rv;
}

ScamValue IndexLikeParameter::checkIndexNature(ScamValue args)
{
    IntegerParameter index;
    ScamValue rv = index.transform(args);
    ScamValue temp = index.value;

    if ( ! isUnhandledError(rv) ) {
        if ( isExact(temp) ) {
            ScamValue check = checkReferences(temp);
            if ( isUnhandledError(check) ) {
                rv = check;
            }
            else {
                valid = true;
                value = index.value;
            }
        }
        else {
            rv = inexactError(temp);
        }
    }

    return rv;
}

ScamValue IndexLikeParameter::checkReferences(ScamValue arg)
{
    int refLen   = length(referant.value);
    int idx      = asInteger(arg);
    ScamValue rv = inbounds(idx, refLen);

    if ( ! isUnhandledError(rv) ) {
        if ( ! truth(rv) ) {
            rv = outOfRange(idx);
        }
        else {
            rv = arg;
        }
    }

    return rv;
}

ScamValue IndexLikeParameter::invalidReferant()
{
    static const char * msg = "referant is invalid";
    ScamValue err = makeError(msg);
    err->errorCategory() = argsCategory;
    return err;
}

ScamValue IndexLikeParameter::outOfRange(int idx)
{
    static const char * msg = "index %{0} out of range for referant (%{1})";
    ScamValue err = makeError(msg, makeInteger(idx, true), referant.value);
    err->errorCategory() = argsCategory;
    return err;
}

IndexParameter::IndexParameter(ObjectParameter & referant)
    : IndexLikeParameter(referant)
{
}

ScamValue IndexParameter::inbounds(int idx, int ref)
{
    return makeBoolean((idx >= 0) && (idx < ref));
}

StartIndexParameter::StartIndexParameter(ObjectParameter & referant)
    : IndexLikeParameter(referant)
{
}

ScamValue StartIndexParameter::inbounds(int idx, int ref)
{
    return makeBoolean((idx >= 0) && (idx <= ref));
}

EndIndexParameter::EndIndexParameter(ObjectParameter & referant,
                                     StartIndexParameter & start)
    : IndexLikeParameter(referant)
    , start(start)
{
}

ScamValue EndIndexParameter::inbounds(int idx, int ref)
{
    ScamValue rv = getStartIndex();

    if ( ! isUnhandledError(rv) ) {
        int idx0 = asInteger(start.value);
        if ( (idx >= 0) && (idx < idx0) ) {
            rv = indexOutOfOrder(idx0, idx);
        }
        else {
            rv = makeBoolean((idx >= idx0) && (idx <= ref));
        }
    }

    return rv;
}

ScamValue EndIndexParameter::getStartIndex()
{
    ScamValue rv = invalidStart();
    ScamValue temp = start.value;

    if ( start.valid && isInteger(temp) && (asInteger(temp) >= 0) ) {
        rv = temp;
    }

    return rv;
}

ScamValue EndIndexParameter::invalidStart()
{
    static const char * msg = "start index is invalid";
    ScamValue err = makeError(msg);
    err->errorCategory() = argsCategory;
    return err;
}

ScamValue EndIndexParameter::indexOutOfOrder(int idx0, int idx1)
{
    static const char * msg = "indices are out of order for referant %{0} %{1}";
    ScamValue err = makeError(msg,
                              makeInteger(idx0, true),
                              makeInteger(idx1, true));
    err->errorCategory() = argsCategory;
    return err;
}

ScamValue ListParameter::check()
{
    return checker(value, isList, "list");
}

MutableParameter::MutableParameter(ObjectParameter & itemizer)
    : itemizer(itemizer)
{
}

ScamValue MutableParameter::transform(ScamValue args)
{
    valid = false;
    value = makeNothing();

    ScamValue rv = itemizer.transform(args);
    if ( ! isUnhandledError(rv) ) {
        if ( isImmutable(itemizer.value) ) {
            rv = mutableError();
        }
        else {
            valid = true;
            value = itemizer.value;
        }
    }

    return rv;
}

ScamValue MutableParameter::mutableError()
{
    static const char * msg = "parameter is not mutable (%{0})";
    ScamValue err = makeError(msg, itemizer.value);
    err->errorCategory() = argsCategory;
    return err;
}

OptionalParameter::OptionalParameter(ObjectParameter & itemizer)
    : found(false)
    , itemizer(itemizer)
{
}

ScamValue OptionalParameter::transform(ScamValue args)
{
    valid = true;
    value = makeNothing();
    found = false;

    ScamValue rv = itemizer.transform(args);
    if ( itemizer.valid && ! isUnhandledError(rv) ) {
        value = itemizer.value;
        found = true;
    }
    else {
        rv = args;
    }

    return rv;
}

CountedParameter::CountedParameter(ObjectParameter & itemizer,
                                   unsigned min,
                                   unsigned max)
    : itemizer(itemizer)
    , min(min)
    , max(max)
{
}

ScamValue CountedParameter::transform(ScamValue args)
{
    valid = false;
    std::vector<ScamValue> local;
    ScamValue rv = makeNothing();

    while ( local.size() < max ) {
        rv = itemizer.transform(args);
        if ( isUnhandledError(rv) ) {
            break;              // not the droids we are looking for
        }
        if ( rv == args ) {
            break;              // no progress
        }
        local.push_back(itemizer.value);
        args = rv;
    }

    unsigned count = local.size();
    if ( count >= min ) {
        valid = true;
        value = makeList(local);
        rv = args;
    }
    else {
        rv = tooFewValues(count);
    }

    return rv;
}

ScamValue CountedParameter::tooFewValues(unsigned count)
{
    static const char * msg =
        "received %{0} values of expected %{1} to %{2}";
    ScamValue err = makeError(msg,
                              makeInteger(count, true),
                              makeInteger(min, true),
                              makeInteger(max, true));
    err->errorCategory() = argsCategory;
    return err;
}

ListOfParameter::ListOfParameter(ObjectParameter & itemizer)
    : itemizer(itemizer)
{
}

ScamValue ListOfParameter::transform(ScamValue args)
{
    valid = false;

    ListParameter arg0;
    ScamValue rv = arg0.transform(args);

    if ( ! isUnhandledError(rv) ) {
        ScamValue arg = arg0.value;
        while ( ! isNull(arg) ) {
            itemizer.valid = false;
            ScamValue temp = itemizer.transform(arg);
            if ( isUnhandledError(temp) ) {
                rv = temp;
                break;
            }
            else if ( temp == arg ) {
                rv = noProgress();
                break;
            }
            else {
                arg = temp;
            }
        }
    }

    if ( ! isUnhandledError(rv) ) {
        valid = true;
        value = arg0.value;
    }

    return rv;
}

ScamValue ListOfParameter::noProgress()
{
    ScamValue err =
        makeError("Internal error: ListOf has neither error nor progress");
    err->errorCategory() = argsCategory;
    return err;
}

AlternativeParameter::AlternativeParameter(ObjectParameter & choice1,
                                           ObjectParameter & choice2)
    : choice1(choice1)
    , choice2(choice2)
{
}

ScamValue AlternativeParameter::transform(ScamValue args)
{
    valid = false;

    ScamValue rv = choice1.transform(args);
    if ( isUnhandledError(rv) ) {
        rv = choice2.transform(args);
        if ( isUnhandledError(rv) ) {
            rv = noMatch();
        }
        else {
            valid = true;
            value = choice2.value;
        }
    }
    else {
        valid = true;
        value = choice1.value;
    }

    return rv;
}

ScamValue AlternativeParameter::noMatch()
{
    ScamValue err = makeError("No alternative matches");
    err->errorCategory() = argsCategory;
    return err;
}

ScamValue scam::errorCheckMsg(const char * name, ScamValue value)
{
    if ( isUnhandledError(value) ) {
        massageError(name, value);
        value->errorCategory() = argsCategory;
    }

    return value;
}

bool scam::errorCheck(const char * name, ScamValue value)
{
    errorCheckMsg(name, value);
    if ( isUnhandledError(value) ) {
        ScamEngine::getEngine().handleError(value);
        return false;
    }
    return true;
}

ScamValue scam::argsToParmsMsg(ScamValue args)
{
    if ( isNull(args) ) {
        return makeNull();
    }

    ScamValue err = makeError("Extra args at end of list");
    err->errorCategory() = argsCategory;
    return err;
}

bool scam::argsToParms(ScamValue args, const char * name)
{
    ScamValue rv = argsToParmsMsg(args);
    if ( isUnhandledError(rv) ) {
        ScamEngine::getEngine().handleError(rv);
        return false;
    }
    return true;
}

