#if ! defined(SCAMINSTANCE_H)
#define SCAMINSTANCE_H 1

#include "expr/ScamExpr.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ScamInstanceAdapter;
    class ScamClass;

    class ScamInstance : public ScamExpr
    {
    private:
        static ScamEnvKeyType parent;

        friend class MemoryManager;

        ScamInstance(const ScamClass * cls, Env * env);
        static ScamInstance* makeInstance(const ScamClass * cls, Env * env);

    public:
        void mark() const override;

        void
        apply(ExprHandle args, Continuation * cont, Env * env) override;

        void setSelf(ExprHandle expr) const override;
        void setParent(ExprHandle expr) const override;

        friend class ScamInstanceAdapter;
    };
}

#endif
