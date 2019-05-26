#if ! defined(SCAMNULL_H)
#define SCAMNULL_H 1

#include "expr/ScamData.hpp"

namespace scam
{
    class ScamNull : public ScamData
    {
    private:
        friend class MemoryManager;
        ScamNull();
        static ScamNull * makeInstance();
    };
}

#endif
