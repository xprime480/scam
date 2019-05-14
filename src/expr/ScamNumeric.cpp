#include "expr/ScamNumeric.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"

#include <cctype>
#include <cmath>
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

    const bool rv = (::fabs(this->realPart() - that->realPart()) < 1e-9 &&
                     ::fabs(this->imagPart() - that->imagPart()) < 1e-9 );
    return rv;
}

bool ScamNumeric::isExact() const
{
    return exact;
}
