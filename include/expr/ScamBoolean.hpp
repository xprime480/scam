#if ! defined(SCAMBOOLEAN_H)
#define SCAMBOOLEAN_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamBoolean : public ScamExpr
    {
    public:
        ScamBoolean(bool value);
        std::string toString() const override;

        bool truth() const override;

        bool isBoolean() const override;

        ExprHandle clone() override;

    private:
        bool value;
    };
}

#endif
