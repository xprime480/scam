#if ! defined(SCAMNUMERIC_H)
#define SCAMNUMERIC_H 1

#include "expr/ScamExpr.hpp"

#include <string>

namespace scam
{
    class NaNType {};
    class NegInfType {};
    class PosInfType {};

    class ScamNumeric : public ScamExpr
    {
    private:
        friend class MemoryManager;

        explicit ScamNumeric(NaNType tag);
        explicit ScamNumeric(NegInfType tag);
        explicit ScamNumeric(PosInfType tag);

        ScamNumeric(ExprHandle real, ExprHandle imag, bool managed = true);
        ScamNumeric(double value, bool exact, bool managed = true);
        ScamNumeric(int num, int den, bool exact, bool managed = true);
        ScamNumeric(int value, bool exact, bool managed = true);

    protected:
        explicit ScamNumeric(bool exact, bool managed = true);

    private:
        static ScamNumeric * makeInstance(NaNType tag);
        static ScamNumeric * makeInstance(NegInfType tag);
        static ScamNumeric * makeInstance(PosInfType tag);

        static ScamNumeric *
        makeInstance(ExprHandle real, ExprHandle imag, bool managed = true);

        static ScamNumeric *
        makeInstance(double value, bool exact, bool managed = true);

        static ScamNumeric *
        makeInstance(int num, int den, bool exact, bool managed = true);

        static ScamNumeric *
        makeInstance(int value, bool exact, bool managed = true);

    public:
        void mark() const override;

        std::string toString() const override;

        bool isNumeric() const override;
        bool isComplex() const override;
        bool isReal() const override;
        bool isRational() const override;
        bool isInteger() const override;

        bool isNaN() const override;
        bool isNegInf() const override;
        bool isPosInf() const override;

        double asDouble() const override;
        std::pair<int, int> asRational() const override;
        int asInteger() const override;

        ConstExprHandle realPart() const override;
        ConstExprHandle imagPart() const override;

        bool equals(ConstExprHandle expr) const override;

        bool isExact() const override;

    private:
        bool exact;
        unsigned long type;
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
    };
}

#endif
