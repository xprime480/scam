#if ! defined(SCAMNUMERIC_H)
#define SCAMNUMERIC_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamNumeric : public ScamExpr
    {
    public:
        bool isNumeric() const override;

        bool equals(ConstExprHandle expr) const override;

    protected:
        virtual double realPart() const = 0;
        virtual double imagPart() const = 0;

    };
}

#endif
