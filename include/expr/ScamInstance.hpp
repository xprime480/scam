#if ! defined(SCAMINSTANCE_H)
#define SCAMINSTANCE_H 1

#include "expr/ScamExpr.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ScamInstanceAdapter;

    class ScamInstance : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamInstance(ExprHandle vars, ExprHandle funs, Env * env);
	
        static ScamInstance* makeInstance(ExprHandle vars,
                                          ExprHandle funs,
                                          Env * env);

    public:
        void mark() const override;

        std::string toString() const override;

        bool hasApply() const override;
        void apply(ExprHandle args, Continuation * cont, Env * env) override;

        bool isProcedure() const override;
        bool isInstance() const override;

        void setSelf(ExprHandle expr) const override;
        void setParent(ExprHandle expr) const override;

        friend class ScamInstanceAdapter;

    private:
        Env *         priv;
        mutable Env * local;
    };
}

#endif
