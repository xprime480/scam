#include "prim/Substitutor.hpp"

#include "expr/EqualityOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "prim/CommonError.hpp"

#include <sstream>

using namespace std;
using namespace scam;

Substitutor::Substitutor(ScamValue answers)
    : answers(answers)
    , helper(makeNull())
{
}

ScamValue Substitutor::resolve_value(ScamValue expr)
{
    ScamValue rv;

    if ( isPair(expr) ) {
        rv = resolve_pair(expr);
    }
    else if ( isVector(expr) ) {
        rv = resolve_vector(expr);
    }
    else if ( isDict(expr) ) {
        rv = resolve_dict(expr);
    }
    else if ( isKeyword(expr) ) {
        rv = resolve_keyword(expr);
    }
    else {
        rv = expr;
    }

    return rv;
}

ScamValue Substitutor::map_value(ScamValue val)
{
    return resolve_value(val);
}

ScamValue Substitutor::resolve_pair(ScamValue expr)
{
    return map_pair(expr);
}

ScamValue Substitutor::resolve_vector(ScamValue expr)
{
    return map_vector(expr);
}

bool Substitutor::have_seen(ScamValue expr)
{
    ScamValue t = helper;
    while ( ! isNull(t) ) {
        if ( equals(nthcar(t, 0), expr) ) {
            return true;
        }
        t = nthcdr(t, 0);
    }

    return false;
}

ScamValue Substitutor::resolve_keyword(ScamValue expr)
{
    if ( have_seen(expr) ) {
        stringstream s;
        s << "Infinite Loop resolving keyword " << writeValue(expr);
        return make_common_error(s.str().c_str());
    }

    helper = makePair(expr, helper);
    ScamValue val = dictGet(answers, expr);
    ScamValue rv  = resolve_value(val);
    helper = nthcdr(helper, 0);
    return rv;
}

ScamValue Substitutor::resolve_dict(ScamValue expr)
{
    return map_dict(expr);
}
