#if ! defined(SCAMINSTANCEADAPTER_H)
#define SCAMINSTANCEADAPTER_H 1

#include "ScamFwd.hpp"

namespace scam
{
    class ScamInstance;

    class ScamInstanceAdapter
    {
    public:
        ScamInstanceAdapter(ScamValue expr);

        Env * getFunctionMap() const;
        Env * getEnv() const;
        ScamValue getParent() const;

    private:
        const ScamInstance * instance;
    };
}

#endif
