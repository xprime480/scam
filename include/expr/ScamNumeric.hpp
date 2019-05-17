#if ! defined(SCAMNUMERIC_H)
#define SCAMNUMERIC_H 1

#include "expr/ScamExpr.hpp"

#include <string>

namespace scam
{
    class ScamNumeric : public ScamExpr
    {
    private:
        friend class MemoryManager;

        ScamNumeric(int value, bool exact, bool managed = true);

    protected:
        explicit ScamNumeric(bool exact, bool managed = true);

    private:
        static ScamNumeric *
        makeInstance(int value, bool exact, bool managed = true);

    public:
        std::string toString() const override;

        bool isNumeric() const override;
        bool isComplex() const override;
        bool isReal() const override;
        bool isRational() const override;
        bool isInteger() const override;

        double toReal() const override;
        std::pair<int, int> toRational() const override;
        int toInteger() const override;

        bool equals(ConstExprHandle expr) const override;

        bool isExact() const override;

    protected:
        virtual double realPart() const;
        virtual double imagPart() const;

    private:
        bool exact;
        int type;

        static constexpr unsigned long ScamNumericComplexBit  { 1 << 0 };
        static constexpr unsigned long ScamNumericRealBit     { 1 << 1 };
        static constexpr unsigned long ScamNumericRationalBit { 1 << 2 };
        static constexpr unsigned long ScamNumericIntegerBit  { 1 << 3 };

        static constexpr unsigned long ScamNumericComplex =
            ScamNumericComplexBit;
        static constexpr unsigned long ScamNumericReal =
            ScamNumericComplex | ScamNumericRealBit;
        static constexpr unsigned long ScamNumericRational =
            ScamNumericReal | ScamNumericRationalBit;
        static constexpr unsigned long ScamNumericInteger =
            ScamNumericRational | ScamNumericIntegerBit;

        int value;
    };
}

#endif
