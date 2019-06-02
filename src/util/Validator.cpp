#include "util/Validator.hpp"

#include "Continuation.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"

using namespace scam;
using namespace std;
using namespace std::placeholders;

namespace
{
    extern ScamValue matchType(const std::string & context,
                               ScamValue args,
                               Continuation * cont,
                               ValidatorResult & results,
                               const std::string & key,
                               const std::string & predType,
                               function<bool(ScamValue)> pred);
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

ScamValue ValidatorResult::get(const std::string & key) const
{
    ScamValue rv = makeNull();

    auto iter = parameters.find(key);
    if ( iter != parameters.end() ) {
        rv = iter->second;
    }

    return rv;
}

void scam::validate(const std::string & context,
                    ScamValue args,
                    Continuation * cont,
                    Callback callback)
{
    ValidatorResult trivial;
    callback(trivial);
}

void scam::validate(const std::string & context,
                    ScamValue args,
                    Continuation * cont,
                    Callback callback,
                    Matcher matcher)
{
    ValidatorResult result;
    ScamValue remainder = matcher(context, args, cont, result);
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

Matcher scam::matchString(const std::string & key)
{
    const string type { "string" };
    const function<bool(ScamValue)> pred { isString };

    auto fn = bind(matchType, _1, _2, _3, _4, key, type, pred);

    Matcher rv(fn);
    return rv;
}

Matcher scam::matchInteger(const std::string & key)
{
    const string type { "integer" };
    const function<bool(ScamValue)> pred { isInteger };
    auto fn = bind(matchType, _1, _2, _3, _4, key, type, pred);

    Matcher rv(fn);
    return rv;
}

namespace
{
    ScamValue matchType(const std::string & context,
                        ScamValue args,
                        Continuation * cont,
                        ValidatorResult & results,
                        const std::string & key,
                        const std::string & predType,
                        function<bool(ScamValue)> pred)
    {
        if ( 0 == length(args) ) {
            results.fail();
            ScamValue err =
                makeErrorExtended(context, ": parameter list too short");
            cont->run(err);
            return args;
        }
        else {
            ScamValue car = getCar(args);
            if ( pred(car) ) {
                results.set(key, car);
                return getCdr(args);
            }
            else {
                results.fail();
                ScamValue err = makeErrorExtended(context,
                                                  ": expected ",
                                                  predType,
                                                  " ",
                                                  "for parm '",
                                                  key,
                                                  "', got '",
                                                  writeValue(car),
                                                  "'");
                cont->run(err);
                return args;
            }
        }
    }
}
