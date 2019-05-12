#include "expr/ScamSpecialNumeric.hpp"

using namespace scam;

ScamSpecialNumeric::ScamSpecialNumeric()
    : ScamExpr(false)
{
}

bool ScamSpecialNumeric::isNumeric() const
{
    return true;
}

bool ScamSpecialNumeric::isExact() const
{
    return false;
}

bool ScamSpecialNumeric::isComplex() const
{
    return true;
}

bool ScamSpecialNumeric::isReal() const
{
    return true;
}
