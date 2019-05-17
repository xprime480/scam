#include "expr/ScamNumeric.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"

#include <cctype>
#include <cmath>
#include <sstream>

using namespace scam;
using namespace std;

ScamNumeric::ScamNumeric(int value, bool exact, bool managed)
    : ScamExpr(managed)
    , exact(exact)
    , type(ScamNumericInteger)
    , value(value)
{
}

ScamNumeric::ScamNumeric(bool exact, bool managed)
    : ScamExpr(managed)
    , exact(exact)
    , type(0)
{
}

ScamNumeric *
ScamNumeric::makeInstance(int value, bool exact, bool managed)
{
    return new ScamNumeric(value, exact, managed);
}

string ScamNumeric::toString() const
{
    stringstream s;
    s << value;
    return s.str();
}

bool ScamNumeric::isNumeric() const
{
    return true;
}

bool ScamNumeric::isComplex() const
{
    return ScamNumericComplexBit == (type & ScamNumericComplexBit);
}

bool ScamNumeric::isReal() const
{
    return ScamNumericRealBit == (type & ScamNumericRealBit);
}

bool ScamNumeric::isRational() const
{
    return ScamNumericRationalBit == (type & ScamNumericRationalBit);
}

bool ScamNumeric::isInteger() const
{
    return ScamNumericIntegerBit == (type & ScamNumericIntegerBit);
}

double ScamNumeric::toReal() const
{
    if ( ! isReal() ) {
        stringstream s;
        s << "Cannot convert <" << this->toString() << "> to real";
        throw ScamException(s.str());
    }

    return (double) value;
}

std::pair<int, int> ScamNumeric::toRational() const
{
    if ( ! isRational() ) {
    }

    int num { value };
    int den { 1 };
    return make_pair<int,int>(move(num), move(den));
}

int ScamNumeric::toInteger() const
{
    if ( ! isInteger() ) {
        stringstream s;
        s << "Cannot convert <" << this->toString() << "> to integer";
        throw ScamException(s.str());
    }

    return value;
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

double ScamNumeric::realPart() const
{
    return (double) value;
}

double ScamNumeric::imagPart() const
{
    return 0.0;
}

