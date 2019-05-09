#if ! defined(SCAMNUMERIC_H)
#define SCAMNUMERIC_H 1

#include "expr/ScamExpr.hpp"

#include <string>

namespace scam
{
    class ScamNumeric : public ScamExpr
    {
    protected:
        explicit ScamNumeric(bool exact);

    public:
        bool isNumeric() const override;

        bool equals(ConstExprHandle expr) const override;

        bool isExact() const override;

    protected:
        virtual double realPart() const = 0;
        virtual double imagPart() const = 0;

    private:
        bool exact;

    };
}

#endif
