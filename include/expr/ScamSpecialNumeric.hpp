#if ! defined(SCAMSPECIALNUMERIC_H)
#define SCAMSPECIALNUMERIC_H 1

#include "expr/ScamExpr.hpp"

#include <string>

namespace scam
{
    class ScamSpecialNumeric : public ScamExpr
    {
    protected:
        ScamSpecialNumeric();

    public:
        bool isNumeric() const override;
        bool isExact() const override;
        bool isComplex() const override;
        bool isReal() const override;
    };
}

#endif
