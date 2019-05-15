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
        ExprHandle getValue() const;
        const char * getPos() const;

    private:
        const char * pos;

        ExprHandle value;

        int base;
        ExactnessType exactness;
        bool exact;
        bool OK;

        void convert();
        bool scanSpecialValue();

        void scanInitial();
        void exactnessSeen(char x);
        void baseSeen(char x);

        int scanInteger();
        int scanSign();
        int scanUnsigned();

        double scanFraction();
        bool scanRadixPoint();

        int scanExponent();

        void finalizeExactness();
        void makeResult(int integerPart, double fractionalPart, int exponent);
        double makeMultiplier(int exponent) const;

        int convertDigit(char digit);
    };
}

#endif
