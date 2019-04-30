#if ! defined(SCAMINSTANCEADAPTER_H)
#define SCAMINSTANCEADAPTER_H 1

#include "ScamFwd.hpp"

namespace scam
{
    class ScamInstance;

    class ScamInstanceAdapter
    {
    public:
        ScamInstanceAdapter(ExprHandle expr);

        Env * getFunctionMap() const;
        Env * getEnv() const;
        ExprHandle getParent() const;

    private:
        const ScamInstance * instance;
    };
}

#endif
