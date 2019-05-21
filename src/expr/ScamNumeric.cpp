#include "expr/ScamNumeric.hpp"

#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "util/NumericUtils.hpp"

#include <cctype>
#include <cmath>
#include <sstream>

#include "util/DebugTrace.hpp"

using namespace scam;
using namespace std;

#define NUMERIC(data) ((data).value.numericValue)
#define EXACT(data) (NUMERIC(data).exact)

#define REALPART(data) (NUMERIC(data).value.complexValue.real)
#define IMAGPART(data) (NUMERIC(data).value.complexValue.imag)

#define REALVAL(data) (NUMERIC(data).value.realValue)

#define NUMPART(data) (NUMERIC(data).value.rationalValue.num)
#define DENPART(data) (NUMERIC(data).value.rationalValue.den)

#define INTVAL(data) (NUMERIC(data).value.intValue)

namespace
{
    bool isPureComplex(ConstExprHandle expr)
    {
        return expr->isComplex() && ! expr->isReal();
    }
}

ScamNumeric::ScamNumeric(ScamData::NaNType tag)
    : ScamExpr(false)
{
    data.type = ScamData::NaN;
    EXACT(data) = false;
}

ScamNumeric::ScamNumeric(ScamData::NegInfType tag)
    : ScamExpr(false)
{
    data.type = ScamData::NegInf;
    EXACT(data) = false;
}

ScamNumeric::ScamNumeric(ScamData::PosInfType tag)
    : ScamExpr(false)
{
    data.type = ScamData::PosInf;
    EXACT(data) = false;
}

ScamNumeric::ScamNumeric(ExprHandle real, ExprHandle imag, bool managed)
    : ScamExpr(managed)
{
    data.type  = ScamData::Complex;
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
    : ScamExpr(managed)
{
    data.type = ScamData::Real;

    EXACT(data) = exact;
    REALVAL(data) = value;
}

ScamNumeric::ScamNumeric(int num, int den, bool exact, bool managed)
    : ScamExpr(managed)
{
    EXACT(data) = exact;
    data.type  = ScamData::Rational;

    const int div = gcd(num, den);
    NUMPART(data) = num / div;
    DENPART(data) = den / div;
}

ScamNumeric::ScamNumeric(int value, bool exact, bool managed)
    : ScamExpr(managed)
{
    data.type = ScamData::Integer;

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

string ScamNumeric::toString() const
{
    stringstream s;

    if ( isNaN() ) {
        s << "+nan.0";
    }
    else if ( isNegInf() ) {
        s << "-inf.0";
    }
    else if ( isPosInf() ) {
        s << "+inf.0";
    }
    else if ( isInteger() ) {
        s << INTVAL(data);
    }
    else if ( isRational() ) {
        s << NUMPART(data) << "/" << DENPART(data);
    }
    else if ( isReal() ) {
        s << REALVAL(data);
    }
    else if ( isComplex() ) {
        //
        // The complexity is so that the output is in the simplest
        // form that will be read by the scanner as the same value.
        // For example, An imaginary part of "-1i" is equivalent to
        // "-i", so the latter is used for the representation.  The
        // real pa
        //
        ExprHandle r { REALPART(data) };
        ExprHandle i { IMAGPART(data) };

        if ( ! r->isInteger() || 0 != r->asInteger() ) {
            s << r->toString();
        }

        const string irepr = i->toString();
        if ( irepr == "0" ) {
            // nothing
        }
        if ( irepr == "1" ) {
            s << "+";
        }
        else if ( irepr == "-1" ) {
            s << "-";
        }
        else {
            const char lead = *irepr.c_str();
            if ( ('+' != lead) && ('-' != lead) ) {
                s << "+";
            }
            s << irepr;
        }
         s << "i";
    }
    else {
        s << "@obj<" << this << ">";
    }

    return s.str();
}

double ScamNumeric::asDouble() const
{
    if ( ! isReal() || isNaN() || isNegInf() || isPosInf() ) {
        stringstream s;
        s << "Cannot convert <" << this->toString() << "> to real";
        throw ScamException(s.str());
    }

    if ( isInteger() ) {
        return (double) INTVAL(data);
    }
    else if ( isRational() ) {
        return ((double) NUMPART(data) / (double) DENPART(data) );
    }
    else if ( isNaN() || isNegInf() || isPosInf() ) {
        // drop through to error case;
    }
    else if ( isReal() ) {
        return REALVAL(data);
    }

    stringstream s;
    s << "Cannot convert <" << this->toString() << "> to double";
    throw ScamException(s.str());

    return 0.0;
}

std::pair<int, int> ScamNumeric::asRational() const
{
    int num { 0 };
    int den { 1 };

    if ( isInteger() ) {
        num = INTVAL(data);
    }
    else if ( isRational() ) {
        num = NUMPART(data);
        den = DENPART(data);
    }
    else {
        stringstream s;
        s << "Cannot convert <" << this->toString() << "> to rational";
        throw ScamException(s.str());
    }

    return make_pair<int,int>(move(num), move(den));
}

int ScamNumeric::asInteger() const
{
    if ( ! isInteger() ) {
        stringstream s;
        s << "Cannot convert <" << this->toString() << "> to integer";
        throw ScamException(s.str());
    }

    return INTVAL(data);
}

bool ScamNumeric::equals(ConstExprHandle expr) const
{
    if ( ! expr->isNumeric() ) {
        return false;
    }

    if ( isNaN() || expr->isNaN() ) {
        return isNaN() && expr->isNaN();
    }
    if ( isNegInf() || expr->isNegInf() ) {
        return isNegInf() && expr->isNegInf();
    }
    if ( isPosInf() || expr->isPosInf() ) {
        return isPosInf() && expr->isPosInf();
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

bool ScamNumeric::isExact() const
{
    return EXACT(data);
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
