#if ! defined(PRIMITIVE_H)
#define PRIMITIVE_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class Primitive : public ScamExpr
    {
    public:
        Primitive(std::string const & name);

        std::string toString() const override;

        bool hasApply() const override;
        void apply(ScamExpr * args, Continuation * cont, Env env) override;

        virtual void applyArgs(ScamExpr * args, Continuation * cont) = 0;

    protected:
        std::string const name;
    };
}

#endif
