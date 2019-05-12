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

        enum class ScanState : unsigned char
            {
             SS_BEGIN,
             SS_BASE,
             SS_EXACT,
             SS_BASE_AND_EXACT,
             SS_SIGN,
             SS_INTEGER_PART,
             SS_RADIX_POINT,
             SS_FRACTIONAL_PART,
             SS_DONE,
             SS_ERROR = 255
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
        bool negative;
        bool exact = true;
        double integerPart;
        double fractionalPart;
        double divisor;
        ScanState state;

        void convert();
        bool scanSpecialValue();
        void scanInitial();
        void exactnessSeen(char x);
        void baseSeen(char x);
        void handleChar(char c);
        bool scanIntegerPart();
        bool scanFractionalPart();
        void finalizeExactness();
        void makeResult();

        int convertDigit(char digit);
    };
}

#endif
