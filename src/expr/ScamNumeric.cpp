#include "expr/ScamNumeric.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"

#include <cctype>
#include <sstream>

using namespace scam;
using namespace std;

ScamNumeric::ScamNumeric(bool exact, bool managed)
    : ScamExpr(managed)
    , exact(exact)
{
}

bool ScamNumeric::isNumeric() const
{
    return true;
}

bool ScamNumeric::equals(ConstExprHandle expr) const
{
    if ( ! expr->isNumeric() ) {
        return false;
    }

    const ScamNumeric * that = dynamic_cast<const ScamNumeric *>(expr);

    return ( ::abs(this->realPart() - that->realPart()) < 1e-9 &&
             ::abs(this->imagPart() - that->imagPart()) < 1e-9 );
}

bool ScamNumeric::isExact() const
{
    return exact;
}
