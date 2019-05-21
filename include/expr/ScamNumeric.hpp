#if ! defined(SCAMNUMERIC_H)
#define SCAMNUMERIC_H 1

#include "expr/ScamExpr.hpp"
#include "expr/ScamData.hpp"

#include <string>

namespace scam
{
    class ScamNumeric : public ScamExpr
    {
    private:
        friend class MemoryManager;

        explicit ScamNumeric(ScamData::NaNType tag);
        explicit ScamNumeric(ScamData::NegInfType tag);
        explicit ScamNumeric(ScamData::PosInfType tag);

        ScamNumeric(ExprHandle real, ExprHandle imag, bool managed = true);
        ScamNumeric(double value, bool exact, bool managed = true);
        ScamNumeric(int num, int den, bool exact, bool managed = true);
        ScamNumeric(int value, bool exact, bool managed = true);

    protected:
        explicit ScamNumeric(bool exact, bool managed = true);

    private:
        static ScamNumeric * makeInstance(ScamData::NaNType tag);
        static ScamNumeric * makeInstance(ScamData::NegInfType tag);
        static ScamNumeric * makeInstance(ScamData::PosInfType tag);

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

        double asDouble() const override;
        std::pair<int, int> asRational() const override;
        int asInteger() const override;

        ConstExprHandle realPart() const override;
        ConstExprHandle imagPart() const override;

        bool equals(ConstExprHandle expr) const override;
    };
}

#endif
