#if ! defined(SCAMINSTANCE_H)
#define SCAMINSTANCE_H 1

#include "Env.hpp"

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamInstanceAdapter;

    class ScamInstance : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamInstance(ScamExpr * vars, ScamExpr * funs, Env env);
        static ScamInstance* makeInstance(ScamExpr * vars,
                                          ScamExpr * funs,
                                          Env env);

    public:
        std::string toString() const override;

        bool hasApply() const override;
        void apply(ScamExpr * args, ContHandle cont, Env env) override;

        bool isProcedure() const override;
        bool isInstance() const override;

        void setSelf(ScamExpr * expr) const override;
        void setParent(ScamExpr * expr) const override;

        friend class ScamInstanceAdapter;

    private:
        Env         priv;
        mutable Env local;
    };
}

#endif
