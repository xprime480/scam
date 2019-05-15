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

        static ExprHandle simplify(ExprHandle value);

        ExprHandle getValue() const;
        const char * getPos() const;

    private:
        const char * pos;

        ExprHandle value;

        int base;
        ExactnessType exactness;

        void scanNum();
        void scanComplex();
        ExprHandle scanReal();
        ExprHandle scanUReal();
        ExprHandle scanDecimal();
        ExprHandle scanUInteger();
        void scanPrefix();
        ExprHandle scanInfNan();
        ExprHandle scanSuffix();

        int scanSign();

        void exactnessSeen(char x);
        void baseSeen(char x);
        ExprHandle makeFraction(unsigned minCount);

        bool scanRadixPoint();

        double makeMultiplier(int exponent) const;
        int convertDigit(char digit) const;

        ExprHandle makeRealWithExactness(double value) const;
        ExprHandle makeRationalWithExactness(int num, int den) const;
        ExprHandle makeIntegerWithExactness(int value) const;
    };
}

#endif
