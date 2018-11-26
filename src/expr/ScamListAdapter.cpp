
#include "expr/ScamListAdapter.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamCons.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamListAdapter::ScamListAdapter(ScamExpr const * expr)
    : lst(dynamic_cast<ScamCons const *>(expr))
    , isNil(expr->isNil())
    , len(0u)
{
    if ( ! expr->isList() ) {
        stringstream s;
        s << "ScamListAdapter expected a list, got: " << expr->toString();
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

ExprHandle ScamListAdapter::append(ScamExpr * tail) const
{
    if ( isNil ) {
        return ExpressionFactory::makeList(tail);
    }

    ScamListAdapter a(cdr.get());
    ExprHandle newCdr = a.append(tail);
    return ExpressionFactory::makeCons(car.get(), newCdr.get());
}
