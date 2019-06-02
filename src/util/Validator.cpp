#include "util/Validator.hpp"

#include "Continuation.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"

#include <sstream>

using namespace scam;
using namespace std;
using namespace std::placeholders;

namespace
{
    using SimplePredicate = function<bool(ScamValue)>;
    using PredicateType   = function<bool(ScamValue, MatcherControl&)>;

    extern Matcher buildTypeMatcher(const string & key,
                                    const string & description,
                                    const PredicateType pred);

    extern Matcher buildTypeMatcher(const string & key,
                                    const string & description,
                                    const SimplePredicate pred);

    extern ScamValue typeMatcher(MatcherControl & control,
                                 const string & key,
                                 const string & description,
                                 const PredicateType pred);

    extern ScamValue sequenceMatcher(MatcherControl & control,
                                     const string & key,
                                     Matcher m1,
                                     Matcher m2);

    extern ScamValue alternativeMatcher(MatcherControl & control,
                                        const string & key,
                                        Matcher m1,
                                        Matcher m2);

    extern ScamValue countMatcher(MatcherControl & control,
                                  const string & key,
                                  const string & itemKey,
                                  Matcher m,
                                  unsigned min,
                                  unsigned max);

    extern ScamValue sublistMatcher(MatcherControl & control,
                                    const string & key,
                                    const string & itemKey,
                                    Matcher m);
}

ValidatorResult::ValidatorResult()
    : status(true)
{
}

void ValidatorResult::fail()
{
    status = false;
}

ValidatorResult::operator bool() const
{
    return status;
}

void ValidatorResult::set(const string & key, ScamValue value)
{
    parameters[key] = value;
}

ScamValue ValidatorResult::get(const string & key) const
{
    ScamValue rv = makeNull();

    auto iter = parameters.find(key);
    if ( iter != parameters.end() ) {
        rv = iter->second;
    }

    return rv;
}

void scam::validate(const string & context,
                    ScamValue args,
                    Continuation * cont,
                    Callback callback)
{
    ValidatorResult trivial;
    callback(trivial);
}

void scam::validate(const string & context,
                    ScamValue args,
                    Continuation * cont,
                    Callback callback,
                    Matcher matcher)
{
    ValidatorResult result;
    MatcherControl control { context,
                             args,
                             cont,
                             result };

    ScamValue remainder = matcher(control);

    if ( result ) {
        if ( isNil(remainder) ) {
            callback(result);
        }
        else {
            result.fail();
            ScamValue err = makeErrorExtended(context,
                                              ": excess arguments found: '",
                                              writeValue(remainder),
                                              "'" );
            cont->run(err);
        }
    }
}

Matcher scam::matchCharacter(const string & key)
{
    return buildTypeMatcher(key, "character", isChar);
}

Matcher scam::matchInteger(const string & key)
{
    return buildTypeMatcher(key, "integer", isInteger);
}

Matcher scam::matchNonNegativeInteger(const string & key)
{
    auto pred = [] (ScamValue value) -> bool
    {
        return isInteger(value) && (asInteger(value) >= 0);
    };

    return buildTypeMatcher(key, "non-negative integer", pred);
}

Matcher scam::matchString(const string & key)
{
    return buildTypeMatcher(key, "string", isString);
}

Matcher scam::matchSymbol(const string & key)
{
    return buildTypeMatcher(key, "symbol", isSymbol);
}

Matcher scam::matchIndex(const std::string & key, const std::string & itemKey)
{
    auto pred = [itemKey] (ScamValue value, MatcherControl & control) -> bool
    {
        if ( ! isInteger(value) || ! isExact(value) ) {
            return false;
        }

        int index = asInteger(value);
        ScamValue item = control.results.get(itemKey);
        int len = length(item);
        return index >= 0 && index < len;
    };

    return buildTypeMatcher(key, "index", pred);
}


Matcher scam::matchSequence(const string & key, Matcher m1, Matcher m2)
{
    auto fn = bind(sequenceMatcher, _1, key, m1, m2);

    Matcher rv(fn);
    return rv;
}

Matcher scam::matchAlternative(const std::string & key, Matcher m1, Matcher m2)
{
    auto fn = bind(alternativeMatcher, _1, key, m1, m2);

    Matcher rv(fn);
    return rv;
}

Matcher scam::matchCount(const string & key,
                         const string & itemKey,
                         Matcher m,
                         unsigned min,
                         unsigned max)
{
    auto fn = bind(countMatcher, _1, key, itemKey, m, min, max);

    Matcher rv(fn);
    return rv;
}

Matcher scam::matchSublist(const string & key,
                           const string & itemKey,
                           Matcher m)
{
    auto fn = bind(sublistMatcher, _1, key, itemKey, m);

    Matcher rv(fn);
    return rv;
}

namespace
{
    extern Matcher buildTypeMatcher(const string & key,
                                    const string & description,
                                    const PredicateType pred)
    {
        auto fn = bind(typeMatcher, _1, key, description, pred);

        Matcher rv(fn);
        return rv;
    }

    extern Matcher buildTypeMatcher(const string & key,
                                    const string & description,
                                    const SimplePredicate pred)
    {
        auto wrapper = [pred] (ScamValue value, MatcherControl & _) -> bool
        {
            return pred(value);
        };

        return buildTypeMatcher(key, description, wrapper);
    }

    ScamValue typeMatcher(MatcherControl & control,
                          const string & key,
                          const string & description,
                          const PredicateType pred)
    {
        if ( 0 == length(control.args) ) {
            control.results.fail();
            ScamValue err = makeErrorExtended(control.context,
                                              ": no value for parameter ",
                                              key);
            control.cont->run(err);
            return control.args;
        }
        else {
            ScamValue car = getCar(control.args);
            if ( pred(car, control) ) {
                control.results.set(key, car);
                return getCdr(control.args);
            }
            else {
                control.results.fail();
                ScamValue err = makeErrorExtended(control.context,
                                                  ": expected ",
                                                  description,
                                                  " for parm '",
                                                  key,
                                                  "', got '",
                                                  writeValue(car),
                                                  "'");
                control.cont->run(err);
                return control.args;
            }
        }
    }

    ScamValue sequenceMatcher(MatcherControl & control,
                              const string & key,
                              Matcher m1,
                              Matcher m2)
    {
        ScamValue rest = m1(control);
        if ( control.results ) {
            MatcherControl control2 { control.context,
                                      rest,
                                      control.cont,
                                      control.results };
            rest = m2(control2);
        }
        return rest;
    }

    ScamValue alternativeMatcher(MatcherControl & control,
                                 const string & key,
                                 Matcher m1,
                                 Matcher m2)
    {
        ScamValue rest;
        Continuation * ignore =
            standardMemoryManager.make<Continuation>("Ignore");

        ValidatorResult temp1;
        MatcherControl control1 = { control.context,
                                    control.args,
                                    ignore,
                                    temp1 };
        rest = m1(control1);
        if ( temp1 ) {
            control.results = temp1;
            return rest;
        }

        ValidatorResult temp2;
        MatcherControl control2 = { control.context,
                                    control.args,
                                    ignore,
                                    temp2 };
        rest = m2(control2);
        if ( temp2 ) {
            control.results = temp2;
            return rest;
        }

        control.results.fail();
        ScamValue err = makeErrorExtended(control.context,
                                          ": no alternative was accepted ",
                                          "for input '",
                                          writeValue(control.args),
                                          "'");
        control.cont->run(err);
        return control.args;
    }

    ScamValue countMatcher(MatcherControl & control,
                           const string & key,
                           const string & itemKey,
                           Matcher m,
                           unsigned min,
                           unsigned max)
    {
        vector<ScamValue> items;
        unsigned matched { 0 };
        Continuation * ignore =
            standardMemoryManager.make<Continuation>("CountMatcher");
        ScamValue rest = control.args;

        while ( matched < max ) {
            ValidatorResult tempResults;
            MatcherControl tempControl { control.context,
                                         rest,
                                         ignore,
                                         tempResults };
            rest = m(tempControl);
            if ( ! tempResults ) {
                break;
            }

            ScamValue value = tempResults.get(itemKey);
            items.push_back(value);
            ++matched;
        }

        if ( matched < min ) {
            control.results.fail();
            ScamValue err = makeErrorExtended(control.context,
                                              ": ",
                                              matched,
                                              " values found, wanted at least ",
                                              min,
                                              " for key ",
                                              key,
                                              " input '",
                                              writeValue(control.args),
                                              "'");
            control.cont->run(err);
            return control.args;
        }

        control.results.set(key, makeList(items));
        return rest;
    }

    ScamValue sublistMatcher(MatcherControl & control,
                             const string & key,
                             const string & itemKey,
                             Matcher m)
    {
        if ( isNil(control.args) || ! isList(getCar(control.args)) ) {
            control.results.fail();
            ScamValue err = makeErrorExtended(control.context,
                                              ": no sublist found for key ",
                                              key,
                                              " input '",
                                              writeValue(control.args),
                                              "'");
            control.cont->run(err);
            return control.args;
        }

        ValidatorResult temp2;
        ScamValue car = getCar(control.args);
        MatcherControl control2 { control.context,
                                  car,
                                  control.cont,
                                  temp2 };
        ScamValue rest = m(control2);
        if ( temp2 ) {
            if ( isNil(rest) ) {
                control.results.set(key, car);
                return getCdr(control.args);
            }

            control.results.fail();
            ScamValue err = makeErrorExtended(control.context,
                                              ": excess parameters in sublist"
                                              " for key ",
                                              key,
                                              " input '",
                                              writeValue(rest),
                                              "'");
            control.cont->run(err);
        }

        return control.args;
    }
}
