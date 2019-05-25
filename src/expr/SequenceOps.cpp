#include "expr/SequenceOps.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamValue scam::getCar(ScamValue value)
{
    if ( ! isCons(value) ) {
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
    if ( ! isCons(value) ) {
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
    else if ( isCons(value) ) {
        ScamValue cdr = CDR(value);
        size_t len = 1;
        if ( isCons(cdr) ) {
            len += length(cdr);
        }
        else if ( ! isNil(cdr) ) {
            len += 1;
        }

        rv = len;
    }
    else if ( isDict(value) ) {
        rv = DICTKEYS(value).size();
    }
    else if ( isNil(value) ) {
        rv = 0u;
    }
    else if ( isVector(value) ) {
        rv = VECTOR(value).size();
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
    ScamValue rv = ExpressionFactory::makeNull();

    const size_t len = length(value);
    if ( n >= len ) {
        rv = ExpressionFactory::makeError("Requested index ",
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
    ScamValue rv = ExpressionFactory::makeNull();

    if ( isByteVector(value) ) {
        rv = checkLength(value, n);
        if ( ! error(rv) ) {
            rv = ExpressionFactory::makeInteger(BYTEVECTOR(value)[n], true);
        }
    }
    else if ( isCons(value) ) {
        rv = checkLength(value, n);
        if ( ! error(rv) ) {
            ScamValue cdr = CDR(value);

            if ( 0 == n ) {
                rv = CAR(value);
            }
            else if ( isCons(cdr) ) {
                rv = nthcar(cdr, n-1);
            }
            else {
                rv = cdr;
            }
        }
    }
    else if ( isVector(value) ) {
        rv = checkLength(value, n);
        if ( ! error(rv) ) {
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
    if ( ! isCons(value) ) {
        stringstream s;
        s << "Cannot index <" << writeValue(value) << ">";
        s << " at " << __FILE__ << ":" << __LINE__;
        auto msg = s.str();
        throw ScamException(msg);
    }

    auto outOfRange = ExpressionFactory::makeError("Index ",
						   n,
						   " requested for ",
						   writeValue(value));
    if ( n >= length(value) ) {
        return outOfRange;
    }

    ScamValue cdr = CDR(value);
    ScamValue rv;

    if ( 0 == n ) {
        rv = cdr;
    }
    else if ( isCons(cdr) ) {
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

    if ( isNil(expr) ) {
        return ExpressionFactory::makeList(tail);
    }

    /* by assumption, isCons! */

    ScamValue car = nthcar(expr, 0);
    ScamValue cdr = nthcdr(expr, 0);
    ScamValue newCdr = append(cdr, tail);
    return ExpressionFactory::makeCons(car, newCdr);
}
