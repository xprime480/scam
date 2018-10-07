#if ! defined(SCAMNULL_H)
#define SCAMNULL_H 1

#include "expr/ScamExpr.hpp"

#include <string>

namespace scam
{
    class ScamNull : public ScamExpr
    {
    public:
        std::string toString() const override;
        void eval(ContHandle cont, Env & env) override;

        bool isNull() const override;
        bool truth() const override;

        ExprHandle clone() override;
    };
}

#endif
