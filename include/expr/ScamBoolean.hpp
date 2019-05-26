#if ! defined(SCAMBOOLEAN_H)
#define SCAMBOOLEAN_H 1

#include "expr/ScamData.hpp"

namespace scam
{
    class ScamBoolean : public ScamData
    {
    private:
        friend class MemoryManager;
        ScamBoolean(bool value);
        static ScamBoolean * makeInstance(bool value);
    };
}

#endif
