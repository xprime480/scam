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
        static bool isNumeric(const ScamData * data);
        static bool isExact(const ScamData * data);
        static bool isComplex(const ScamData * data);
        static bool isPureComplex(const ScamData * data);
        static bool isReal(const ScamData * data);
        static bool isRational(const ScamData * data);
        static bool isInteger(const ScamData * data);
        static bool isNaN(const ScamData * data);
        static bool isNegInf(const ScamData * data);
        static bool isPosInf(const ScamData * data);
        static double asDouble(const ScamData * data);
        static std::pair<int, int> asRational(const ScamData * data);
        static int asInteger(const ScamData * data);

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

    protected:
        explicit ScamNumeric(bool exact, bool managed = true);

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
