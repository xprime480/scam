#if ! defined(SCAMFLOAT_H)
#define SCAMFLOAT_H 1

#include "expr/ScamNumeric.hpp"

#include <string>

namespace scam
{
    class ScamFloat : public ScamNumeric
    {
    protected:
        ScamFloat(double value);

    public:
        static ScamFloat * makeInstance(double value);

        std::string toString() const override;

        bool isFloat() const override;
        double toFloat() const override;

        bool equals(ScamExpr const * expr) const override;

    private:
        double const value;
    };
}

#endif
