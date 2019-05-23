#include "expr/ScamListAdapter.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamCons.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamListAdapter::ScamListAdapter(ConstScamValue expr)
    : lst(dynamic_cast<ScamCons const *>(expr))
    , isNil(TypePredicates::isNil(expr))
    , len(0u)
{
    if ( ! TypePredicates::isList(expr) ) {
        stringstream s;
        s << "ScamListAdapter expected a list, got: " << writeValue(expr);
        throw ScamException(s.str());
    }

    if ( ! lst ) {
        return;
    }

    len = lst->length();
    car = lst->nthcar(0);
    cdr = lst->nthcdr(0);
}

size_t ScamListAdapter::getLength() const
{
    return len;
}

ScamValue ScamListAdapter::append(ScamValue tail) const
{
    if ( isNil ) {
        return ExpressionFactory::makeList(tail);
    }

    ScamListAdapter a(cdr);
    ScamValue newCdr = a.append(tail);
    return ExpressionFactory::makeCons(car, newCdr);
}
