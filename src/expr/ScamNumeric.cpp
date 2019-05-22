#include "expr/ScamNumeric.hpp"

#include "ScamException.hpp"
#include "expr/ExprWriter.hpp"
#include "expr/ExpressionFactory.hpp"
#include "util/NumericUtils.hpp"

#include <cctype>
#include <cmath>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    bool isPureComplex(const ScamData & data)
    {
        return ScamNumeric::isComplex(data) && ! ScamNumeric::isReal(data);
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

double ScamNumeric::asDouble(const ScamData & data)
{
    if ( ! isReal(data) ) {
        stringstream s;
        s << "Cannot convert <" << ExprWriter::write(data) << "> to double";
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

    return 0.0;
}

std::pair<int, int> ScamNumeric::asRational(const ScamData & data)
{
    if ( ! isRational(data) ) {
        stringstream s;
        s << "Cannot convert <" << ExprWriter::write(data) << "> to rational";
        throw ScamException(s.str());
    }

    int num { 0 };
    int den { 1 };

    if ( isInteger(data) ) {
        num = INTVAL(data);
    }
    else {
        num = NUMPART(data);
        den = DENPART(data);
    }

    return make_pair<int,int>(move(num), move(den));
}

int ScamNumeric::asInteger(const ScamData & data)
{
    if ( ! isInteger(data) ) {
        stringstream s;
        s << "Cannot convert <" << ExprWriter::write(data) << "> to integer";
        throw ScamException(s.str());
    }

    return INTVAL(data);
}

ConstExprHandle ScamNumeric::realPart(const ScamData & data)
{
    if ( ! isNumeric(data) ) {
        stringstream s;
        s << "<" << ExprWriter::write(data)
          << "> is not numeric; has no real part";
        throw ScamException(s.str());
    }

    if ( isPureComplex(data) ) {
        return REALPART(data);
    }

    return ExpressionFactory::makeNull(); // temporary hack!!
}

ConstExprHandle ScamNumeric::imagPart(const ScamData & data)
{
    if ( ! isNumeric(data) ) {
        stringstream s;
        s << "<" << ExprWriter::write(data)
          << "> is not numeric; has no imaginary part";
        throw ScamException(s.str());
    }

    if ( isPureComplex(data) ) {
        return IMAGPART(data);
    }

    return ExpressionFactory::makeInteger(0, true);
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

    if ( isPureComplex(real->getData()) || isPureComplex(imag->getData()) ) {
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

        if ( isPureComplex(data) ) {
            REALPART(data)->mark();
            IMAGPART(data)->mark();
        }
    }
}

bool ScamNumeric::equals(ConstExprHandle expr) const
{
    const ScamData & that = expr->getData();

    if ( ! isNumeric(that) ) {
        return false;
    }

    if ( isNaN(data) || isNaN(that) ) {
        return isNaN(data) && isNaN(data);
    }
    if ( isNegInf(data) || isNegInf(that) ) {
        return isNegInf(data) && isNegInf(data);
    }
    if ( isPosInf(data) || isPosInf(that) ) {
        return isPosInf(data) && isPosInf(that);
    }

    /* temporary hack!!! */
    ConstExprHandle thisH = realPart(data);
    ConstExprHandle thatH = realPart(that);
    
    const double thisR = asDouble(thisH->isNull() ? data : thisH->getData());
    const double thatR = asDouble(thatH->isNull() ? that : thatH->getData());

    if ( ::fabs(thisR- thatR) > 1e-9 ) {
        return false;
    }

    if ( isPureComplex(data) || isPureComplex(that) ) {
        const double thisI = imagPart(data)->asDouble();
        const double thatI = imagPart(that)->asDouble();
        if ( ::fabs(thisI- thatI) > 1e-9 ) {
            return false;
        }
    }

    return true;
}
