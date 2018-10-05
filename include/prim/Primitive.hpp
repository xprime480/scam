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
        void apply(std::shared_ptr<ScamExpr> const & args,
                   std::shared_ptr<Continuation> cont,
                   Env & env) override;

    protected:
        virtual void applyArgs(std::shared_ptr<ScamExpr> const & args,
                               std::shared_ptr<Continuation> cont) = 0;

    private:
        std::string const name;
    };
}

#endif
