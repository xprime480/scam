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
        void
        apply(ScamValue args, Continuation * cont, Env * env) override;

        void setSelf(ScamValue expr) const override;
        void setParent(ScamValue expr) const override;

        friend class ScamInstanceAdapter;
    };
}

#endif
