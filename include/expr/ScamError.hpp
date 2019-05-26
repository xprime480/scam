#if ! defined(SCAMERROR_H)
#define SCAMERROR_H 1

#include "expr/ScamData.hpp"

#include <string>

namespace scam
{
    class ScamError : public ScamData
    {
    private:
        friend class MemoryManager;
        ScamError(char const * msg, bool managed = true);
        static ScamError * makeInstance(char const * msg, bool managed = true);
    };
}

#endif
