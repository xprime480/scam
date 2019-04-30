#if ! defined(PRIMITIVE_H)
#define PRIMITIVE_H 1

#include "expr/ScamExpr.hpp"

#include <string>

namespace scam
{
    class Primitive : public ScamExpr
    {
    public:
        Primitive(std::string const & name);

        std::string toString() const override;

        bool hasApply() const override;
        void apply(ExprHandle args, Continuation * cont, Env * env) override;

        virtual void
        applyArgs(ExprHandle args, Continuation * cont) = 0;

    protected:
        std::string const name;
    };
}

#endif
