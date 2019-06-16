#include "expr/SequenceOps.hpp"

#include "ScamException.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"
#include "expr/ValueFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamValue scam::getCar(ScamValue value)
{
    if ( ! isPair(value) ) {
        stringstream s;
        s << "Cannot take car of <" << writeValue(value) << ">";
        s << " at " << __FILE__ << ":" << __LINE__;
        auto msg = s.str();
        throw ScamException(msg);
    }

    auto rv = CAR(value);
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

    auto rv = CDR(value);
    return rv;
}

size_t scam::length(ScamValue value)
{
    size_t rv { 0u };

    if ( isByteVector(value) ) {
        rv = BYTEVECTOR(value).size();
    }
    else if ( isPair(value) ) {
        ScamValue cdr = CDR(value);
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
        rv = DICTKEYS(value).size();
    }
    else if ( isNull(value) ) {
        rv = 0u;
    }
    else if ( isVector(value) ) {
        rv = VECTOR(value).size();
    }
    else if ( isString(value) ) {
        rv = STRVAL(value).size();
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
        rv = makeErrorExtended("Requested index ",
                               n,
                               " of a ",
                               len,
                               "-element seaquence",
                               writeValue(value));
    }

    return rv;
}

ScamValue scam::nthcar(ScamValue value, size_t n)
{
    ScamValue rv = makeNothing();

    if ( isByteVector(value) ) {
        rv = checkLength(value, n);
        if ( ! isError(rv) ) {
            rv = makeInteger(BYTEVECTOR(value)[n], true);
        }
    }
    else if ( isPair(value) ) {
        rv = checkLength(value, n);
        if ( ! isError(rv) ) {
            ScamValue cdr = CDR(value);

            if ( 0 == n ) {
                rv = CAR(value);
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
            rv = VECTOR(value)[n];
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

    auto outOfRange =
        makeErrorExtended("Index ", n, " requested for ", writeValue(value));
    if ( n >= length(value) ) {
        return outOfRange;
    }

    ScamValue cdr = CDR(value);
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
