#include "expr/ScamNumeric.hpp"

#include "ScamException.hpp"
#include "expr/ExprWriter.hpp"
#include "expr/ExpressionFactory.hpp"
#include "util/NumericUtils.hpp"

#include <cctype>
#include <cmath>
#include <sstream>

#include "util/DebugTrace.hpp"

using namespace scam;
using namespace std;

namespace
{
    bool isPureComplex(ConstExprHandle expr)
    {
        return expr->isComplex() && ! expr->isReal();
    }
}

bool ScamNumeric::isNumeric(const ScamData & data)
{
    return 0 != (data.type & ScamData::Numeric);
}

bool ScamNumeric::isExact(const ScamData & data)
{
    if ( ! isNumeric(data) ) {
        stringstream s;
        s << "Exactness has no meaning for <" << ExprWriter::write(data) << ">";
        throw ScamException(s.str());
    }

    return EXACT(data);
}

bool ScamNumeric::isComplex(const ScamData & data)
{
    return ScamData::ComplexBit == (data.type & ScamData::ComplexBit);
}

bool ScamNumeric::isReal(const ScamData & data)
{
    return ScamData::RealBit == (data.type & ScamData::RealBit);
}

bool ScamNumeric::isRational(const ScamData & data)
{
    return ScamData::RationalBit == (data.type & ScamData::RationalBit);
}

bool ScamNumeric::isInteger(const ScamData & data)
{
    return ScamData::IntegerBit == (data.type & ScamData::IntegerBit);
}

bool ScamNumeric::isNaN(const ScamData & data)
{
    return ScamData::NaNBit == (data.type & ScamData::NaNBit);
}

bool ScamNumeric::isNegInf(const ScamData & data)
{
    return ScamData::NegInfBit == (data.type & ScamData::NegInfBit);
}

bool ScamNumeric::isPosInf(const ScamData & data)
{
    return ScamData::PosInfBit == (data.type & ScamData::PosInfBit);
}

ScamNumeric::ScamNumeric(ScamData::NaNType tag)
    : ScamExpr(ScamData::NaN, false)
{
    EXACT(data) = false;
}

ScamNumeric::ScamNumeric(ScamData::NegInfType tag)
    : ScamExpr(ScamData::NegInf, false)
{
    EXACT(data) = false;
}

ScamNumeric::ScamNumeric(ScamData::PosInfType tag)
    : ScamExpr(ScamData::PosInf, false)
{
    EXACT(data) = false;
}

ScamNumeric::ScamNumeric(ExprHandle real, ExprHandle imag, bool managed)
    : ScamExpr(ScamData::Complex, managed)
{
    EXACT(data) = real->isExact() && imag->isExact();

    if ( isPureComplex(real) || isPureComplex(imag) ) {
        static string msg =
            "Cannot set either part a complex number to another complex number";
        throw ScamException(msg);
    }

    REALPART(data) = real;
    IMAGPART(data) = imag;
}

ScamNumeric::ScamNumeric(double value, bool exact, bool managed)
    : ScamExpr(ScamData::Real, managed)
{
    EXACT(data) = exact;
    REALVAL(data) = value;
}

ScamNumeric::ScamNumeric(int num, int den, bool exact, bool managed)
    : ScamExpr(ScamData::Rational, managed)
{
    EXACT(data) = exact;

    const int div = gcd(num, den);
    NUMPART(data) = num / div;
    DENPART(data) = den / div;
}

ScamNumeric::ScamNumeric(int value, bool exact, bool managed)
    : ScamExpr(ScamData::Integer, managed)
{
    EXACT(data) = exact;
    INTVAL(data) = value;
}

ScamNumeric * ScamNumeric::makeInstance(ScamData::NaNType tag)
{
    static ScamNumeric instance(tag);
    return &instance;
}

ScamNumeric * ScamNumeric::makeInstance(ScamData::NegInfType tag)
{
    static ScamNumeric instance(tag);
    return &instance;
}

ScamNumeric * ScamNumeric::makeInstance(ScamData::PosInfType tag)
{
    static ScamNumeric instance(tag);
    return &instance;
}

ScamNumeric *
ScamNumeric::makeInstance(ExprHandle real, ExprHandle imag, bool managed)
{
    return new ScamNumeric(real, imag, managed);
}

ScamNumeric *
ScamNumeric::makeInstance(double value, bool exact, bool managed)
{
    return new ScamNumeric(value, exact, managed);
}

ScamNumeric *
ScamNumeric::makeInstance(int num, int den, bool exact, bool managed)
{
    return new ScamNumeric(num, den, exact, managed);
}

ScamNumeric *
ScamNumeric::makeInstance(int value, bool exact, bool managed)
{
    return new ScamNumeric(value, exact, managed);
}

void ScamNumeric::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();

        if ( isPureComplex(this) ) {
            REALPART(data)->mark();
            IMAGPART(data)->mark();
        }
    }
}

double ScamNumeric::asDouble() const
{
    if ( ! isReal(data) || isNaN(data) || isNegInf(data) || isPosInf(data) ) {
        stringstream s;
        s << "Cannot convert <" << this->toString() << "> to real";
        throw ScamException(s.str());
    }

    if ( isInteger(data) ) {
        return (double) INTVAL(data);
    }
    else if ( isRational(data) ) {
        return ((double) NUMPART(data) / (double) DENPART(data) );
    }
    else if ( isNaN(data) || isNegInf(data) || isPosInf(data) ) {
        // drop through to error case;
    }
    else if ( isReal(data) ) {
        return REALVAL(data);
    }

    stringstream s;
    s << "Cannot convert <" << ExprWriter::write(data) << "> to double";
    throw ScamException(s.str());

    return 0.0;
}

std::pair<int, int> ScamNumeric::asRational() const
{
    int num { 0 };
    int den { 1 };

    if ( isInteger(data) ) {
        num = INTVAL(data);
    }
    else if ( isRational(data) ) {
        num = NUMPART(data);
        den = DENPART(data);
    }
    else {
        stringstream s;
        s << "Cannot convert <" << ExprWriter::write(data) << "> to rational";
        throw ScamException(s.str());
    }

    return make_pair<int,int>(move(num), move(den));
}

int ScamNumeric::asInteger() const
{
    if ( ! isInteger(data) ) {
        stringstream s;
        s << "Cannot convert <" << ExprWriter::write(data) << "> to integer";
        throw ScamException(s.str());
    }

    return INTVAL(data);
}

bool ScamNumeric::equals(ConstExprHandle expr) const
{
    if ( ! expr->isNumeric() ) {
        return false;
    }

    if ( isNaN(data) || expr->isNaN() ) {
        return isNaN(data) && expr->isNaN();
    }
    if ( isNegInf(data) || expr->isNegInf() ) {
        return isNegInf(data) && expr->isNegInf();
    }
    if ( isPosInf(data) || expr->isPosInf() ) {
        return isPosInf(data) && expr->isPosInf();
    }

    const ScamNumeric * that = dynamic_cast<const ScamNumeric *>(expr);

    const double thisR = this->realPart()->asDouble();
    const double thatR = that->realPart()->asDouble();
    if ( ::fabs(thisR- thatR) > 1e-9 ) {
        return false;
    }

    if ( isPureComplex(this) || isPureComplex(expr) ) {
        const double thisI = this->imagPart()->asDouble();
        const double thatI = that->imagPart()->asDouble();
        if ( ::fabs(thisI- thatI) > 1e-9 ) {
            return false;
        }
    }

    return true;
}

ConstExprHandle ScamNumeric::realPart() const
{
    if ( isPureComplex(this) ) {
        return REALPART(data);
    }
    return this;
}

ConstExprHandle ScamNumeric::imagPart() const
{
    if ( isPureComplex(this) ) {
        return IMAGPART(data);
    }

    return ExpressionFactory::makeInteger(0, true);
}
