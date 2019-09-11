#include "expr/SequenceOps.hpp"

#include "ScamException.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"
#include "expr/ValueFactory.hpp"

#include <algorithm>
#include <sstream>
#include <vector>

using namespace scam;
using namespace std;

namespace
{
    extern void loopFinderHelper(vector<ScamValue> & shared,
                                 vector<ScamValue> & path,
                                 ScamValue value);
}

ScamValue scam::getCar(ScamValue value)
{
    if ( ! isPair(value) ) {
        stringstream s;
        s << "Cannot take car of <" << writeValue(value) << ">";
        s << " at " << __FILE__ << ":" << __LINE__;
        auto msg = s.str();
        throw ScamException(msg);
    }

    auto rv = value->carValue();
    return rv;
}

ScamValue scam::getCdr(ScamValue value)
{
    if ( ! isPair(value) ) {
        stringstream s;
        s << "Cannot take cdr of <" << writeValue(value) << ">";
        s << " at " << __FILE__ << ":" << __LINE__;
        auto msg = s.str();
        throw ScamException(msg);
    }

    auto rv = value->cdrValue();
    return rv;
}

size_t scam::length(ScamValue value)
{
    size_t rv { 0u };

    if ( isByteVector(value) ) {
        rv = value->byteVectorData().size();
    }
    else if ( isPair(value) ) {
        ScamValue cdr = value->cdrValue();
        size_t len = 1;
        if ( isPair(cdr) ) {
            len += length(cdr);
        }
        else if ( ! isNull(cdr) ) {
            len += 1;
        }

        rv = len;
    }
    else if ( isDict(value) ) {
        rv = value->dictKeys().size();
    }
    else if ( isNull(value) ) {
        rv = 0u;
    }
    else if ( isVector(value) ) {
        rv = value->vectorData().size();
    }
    else if ( isString(value) ) {
        rv = value->stringValue().size();
    }
    else {
        stringstream s;
        s << "Cannot take the length of <" << writeValue(value) << ">";
        s << " at " << __FILE__ << ":" << __LINE__;
        auto msg = s.str();
        throw ScamException(msg);
    }

    return rv;
}

ScamValue checkLength(ScamValue value, size_t n)
{
    ScamValue rv = makeNothing();

    const size_t len = length(value);
    if ( n >= len ) {
        rv = makeError("Index out of range", makeInteger(n, true));
    }

    return rv;
}

ScamValue scam::nthcar(ScamValue value, size_t n)
{
    ScamValue rv = makeNothing();

    if ( isByteVector(value) ) {
        rv = checkLength(value, n);
        if ( ! isError(rv) ) {
            rv = makeInteger(value->byteVectorData()[n], true);
        }
    }
    else if ( isPair(value) ) {
        rv = checkLength(value, n);
        if ( ! isError(rv) ) {
            ScamValue cdr = value->cdrValue();

            if ( 0 == n ) {
                rv = value->carValue();
            }
            else if ( isPair(cdr) ) {
                rv = nthcar(cdr, n-1);
            }
            else {
                rv = cdr;
            }
        }
    }
    else if ( isVector(value) ) {
        rv = checkLength(value, n);
        if ( ! isError(rv) ) {
            rv = value->vectorData()[n];
        }
    }
    else {
        stringstream s;
        s << "Cannot index <" << writeValue(value) << ">";
        s << " at " << __FILE__ << ":" << __LINE__;
        auto msg = s.str();
        throw ScamException(msg);
    }

    return rv;
}

ScamValue scam::nthcdr(ScamValue value, size_t n)
{
    if ( ! isPair(value) ) {
        stringstream s;
        s << "Cannot index <" << writeValue(value) << ">";
        s << " at " << __FILE__ << ":" << __LINE__;
        auto msg = s.str();
        throw ScamException(msg);
    }

    auto outOfRange = makeError("Index out of range", makeInteger(n, true));

    if ( n >= length(value) ) {
        return outOfRange;
    }

    ScamValue cdr = value->cdrValue();
    ScamValue rv;

    if ( 0 == n ) {
        rv = cdr;
    }
    else if ( isPair(cdr) ) {
        rv = nthcdr(cdr, n-1);
    }
    else if ( n >= 1 ) {
        rv = outOfRange;
    }
    else {
        rv = cdr;
    }

    return rv;
}

ScamValue scam::append(ScamValue expr, ScamValue tail)
{
    if ( ! isList(expr) ) {
        stringstream s;
        s << "ScamListAdapter expected a list, got: " << writeValue(expr);
        auto msg = s.str();
        throw ScamException(msg);
    }

    if ( isNull(expr) ) {
        return makeList(tail);
    }

    /* by assumption, isPair! */

    ScamValue car = nthcar(expr, 0);
    ScamValue cdr = nthcdr(expr, 0);
    ScamValue newCdr = append(cdr, tail);
    return makePair(car, newCdr);
}

vector<ScamValue> scam::detectSharedStructure(ScamValue value)
{
    vector<ScamValue> shared;

    if ( isPair(value) ) {
        vector<ScamValue> path;
        path.push_back(value);

        loopFinderHelper(shared, path, getCar(value));
        loopFinderHelper(shared, path, getCdr(value));
    }

    return shared;
}

namespace
{
    void loopFinderHelper(vector<ScamValue> & shared,
                          vector<ScamValue> & path,
                          ScamValue value)
    {
        if ( ! isPair(value) ) {
            return;
        }

        const auto iter = find(path.begin(), path.end(), value);
        if ( iter != path.end() ) {
            const auto iter2 = find(shared.begin(), shared.end(), value);
            if ( iter2 == shared.end() ) {
                shared.push_back(value);
            }
            return;
        }

        path.push_back(value);
        loopFinderHelper(shared, path, getCar(value));
        loopFinderHelper(shared, path, getCdr(value));
        path.pop_back();
    }
}
