#if ! defined(SCAMNIL_H)
#define SCAMNIL_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamNil : public ScamExpr
    {
    public:
        std::string toString() const override;

        bool isNil() const override;
        bool isList() const override;
        size_t length() const override;

        ExprHandle clone() override;
    };
}

#endif
