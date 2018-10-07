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
        void
        apply(ExprHandle const & args, ContHandle cont, Env & env) override;

        virtual void applyArgs(ExprHandle const & args, ContHandle cont) = 0;

    private:
        std::string const name;
    };
}

#endif
