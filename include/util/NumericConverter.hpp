#if ! defined(NUMERICCONVERTER_HPP)
#define NUMERICCONVERTER_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    class NumericConverter
    {
    private:
        enum class ExactnessType : unsigned char
            {
             ET_EXACT,
             ET_INEXACT,
             ET_CONTEXT
            };

    public:
        explicit NumericConverter(const char * pos);

        static ScamValue simplify(ScamValue value);

        ScamValue getValue() const;
        const char * getPos() const;

    private:
        const char * pos;

        ScamValue value;

        int base;
        ExactnessType exactness;

        void scanNum();
        void scanComplex();
        ScamValue scanReal();
        ScamValue scanUReal();
        ScamValue scanDecimal();
        ScamValue scanUInteger();
        void scanPrefix();
        ScamValue scanInfNan();
        ScamValue scanSuffix();

        int scanSign(bool optional = true);

        void exactnessSeen(char x);
        void baseSeen(char x);
        ScamValue makeFraction(unsigned minCount);

        bool scanRadixPoint();

        double makeMultiplier(int exponent) const;
        int convertDigit(char digit) const;

        ScamValue makeComplexPolar(ScamValue r, ScamValue theta) const;
        ScamValue makeRealWithExactness(double value) const;
        ScamValue makeRationalWithExactness(int num, int den) const;
        ScamValue makeIntegerWithExactness(int value) const;
    };
}

#endif
