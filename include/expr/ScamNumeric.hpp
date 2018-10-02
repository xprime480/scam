#if ! defined(SCAMNUMERIC_H)
#define SCAMNUMERIC_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamNumeric : public ScamExpr
    {
    public:
        bool isNumeric() const override;
    };
}

#endif
