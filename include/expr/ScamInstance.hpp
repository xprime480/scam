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
    public:
        static ScamEnvKeyType parent;

    private:

        friend class MemoryManager;

        ScamInstance(const ScamClass * cls, Env * env);
        static ScamInstance* makeInstance(const ScamClass * cls, Env * env);

    public:
        friend class ScamInstanceAdapter;
    };

    extern void setSelf(ScamValue instance, ScamValue expr);
    extern void setParent(ScamValue instance, ScamValue expr);
}

#endif
