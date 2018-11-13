#if ! defined(SCAMINSTANCEADAPTER_H)
#define SCAMINSTANCEADAPTER_H 1

#include "Env.hpp"

namespace scam
{
    class ScamExpr;
    class ScamInstance;

    class ScamInstanceAdapter
    {
    public:
        ScamInstanceAdapter(ScamExpr const * expr);

        Env getFunctionMap() const;
        Env getEnv() const;
        ExprHandle getParent() const;

    private:
        ScamInstance const * instance;
    };
}

#endif
