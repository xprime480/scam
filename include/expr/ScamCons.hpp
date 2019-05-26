#if ! defined(SCAMCONS_H)
#define SCAMCONS_H 1

#include "expr/ScamData.hpp"

namespace scam
{
    class ScamCons : public ScamData
    {
    private:
        friend class MemoryManager;
        ScamCons(ScamValue car, ScamValue cdr);
        static ScamCons * makeInstance(ScamValue car, ScamValue cdr);
    };
}

#endif
