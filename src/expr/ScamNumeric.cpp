#include "expr/ScamNumeric.hpp"

using namespace scam;

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

