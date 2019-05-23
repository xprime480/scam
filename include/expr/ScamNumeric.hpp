#if ! defined(SCAMNUMERIC_H)
#define SCAMNUMERIC_H 1

#include "expr/ScamExpr.hpp"
#include "expr/ScamData.hpp"

#include <string>

namespace scam
{
    class ScamNumeric : public ScamExpr
    {
    public:
        static ConstScamValue realPart(const ScamData * data);
        static ConstScamValue imagPart(const ScamData * data);

    private:
        friend class MemoryManager;

        explicit ScamNumeric(ScamData::NaNType tag);
        explicit ScamNumeric(ScamData::NegInfType tag);
        explicit ScamNumeric(ScamData::PosInfType tag);

        ScamNumeric(ScamValue real, ScamValue imag, bool managed = true);
        ScamNumeric(double value, bool exact, bool managed = true);
        ScamNumeric(int num, int den, bool exact, bool managed = true);
        ScamNumeric(int value, bool exact, bool managed = true);

    private:
        static ScamNumeric * makeInstance(ScamData::NaNType tag);
        static ScamNumeric * makeInstance(ScamData::NegInfType tag);
        static ScamNumeric * makeInstance(ScamData::PosInfType tag);

        static ScamNumeric *
        makeInstance(ScamValue real, ScamValue imag, bool managed = true);

        static ScamNumeric *
        makeInstance(double value, bool exact, bool managed = true);

        static ScamNumeric *
        makeInstance(int num, int den, bool exact, bool managed = true);

        static ScamNumeric *
        makeInstance(int value, bool exact, bool managed = true);

    public:
        bool equals(ConstScamValue expr) const override;
    };
}

#endif
