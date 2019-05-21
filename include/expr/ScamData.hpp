#if ! defined(SCAMDATA_HPP)
#define SCAMDATA_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    struct ScamData
    {
    public:
        /*
         * tags and types for numeric types
         */
        class NaNType {};
        class NegInfType {};
        class PosInfType {};

        constexpr static unsigned long ComplexBit  { 1 << 0 };
        constexpr static unsigned long RealBit     { 1 << 1 };
        constexpr static unsigned long RationalBit { 1 << 2 };
        constexpr static unsigned long IntegerBit  { 1 << 3 };

        constexpr static unsigned long NaNBit      { 1 << 4 };
        constexpr static unsigned long NegInfBit   { 1 << 5 };
        constexpr static unsigned long PosInfBit   { 1 << 6 };

        constexpr static unsigned long Complex  = ComplexBit;
        constexpr static unsigned long Real     = Complex | RealBit;
        constexpr static unsigned long Rational = Real | RationalBit;
        constexpr static unsigned long Integer  = Rational | IntegerBit;
        constexpr static unsigned long NaN      = Real | NaNBit;
        constexpr static unsigned long NegInf   = Real | NegInfBit;
        constexpr static unsigned long PosInf   = Real | PosInfBit;

        /*
         * tags and types for other atoms
         */
        constexpr static unsigned long Null      { 1 << 7 };
        constexpr static unsigned long Boolean   { 1 << 8 };
        constexpr static unsigned long Character { 1 << 9 };

        unsigned long type;

        union
        {
            bool boolValue;

	    char charValue;

            struct
            {
                bool exact;
                union
                {
                    struct
                    {
                        ExprHandle real;
                        ExprHandle imag;
                    }  complexValue;

                    double realValue;

                    struct {
                        int num;
                        int den;
                    } rationalValue;

                    int intValue;
                } value;
            } numericValue ;

        } value;
    };
}

#endif
