#if ! defined(NUMERICCONVERTER_HPP)
#define NUMERICCONVERTER_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    class NumericConverter
    {
    private:
        enum class NumericType : unsigned char
            {
             NT_INTEGER,
             NT_RATIONAL,
             NT_REAL,
             NT_COMPLEX
            };

        enum class ExactnessType : unsigned char
            {
             ET_EXACT,
             ET_INEXACT,
             ET_CONTEXT
            };

    public:
        explicit NumericConverter(const char * pos);
        ExprHandle getValue() const;

    private:
        const char * pos;

        ExprHandle value;

        int base;
        ExactnessType exactness;
        bool negative;
        bool exact = true;
        NumericType type = NumericType::NT_INTEGER;
        double integerPart;
        double fractionalPart;

        void convert();
        void scanInitial();

        bool scanIntegerPart();
        bool scanFractionalPart();
        void finalizeExactness();
        void makeResult();

        int convertDigit(char digit);
    };
}

#endif
