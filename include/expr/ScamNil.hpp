#if ! defined(SCAMNIL_H)
#define SCAMNIL_H 1

#include "expr/ScamData.hpp"

namespace scam
{
    class ScamNil : public ScamData
    {
    private:
        friend class MemoryManager;
        ScamNil();
        static ScamNil * makeInstance();
    };
}

#endif
