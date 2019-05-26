#if ! defined(SCAMKEYWORD_H)
#define SCAMKEYWORD_H 1

#include "expr/ScamData.hpp"

namespace scam
{
    class ScamKeyword : public ScamData
    {
    private:
        friend class MemoryManager;
        ScamKeyword(std::string const & value, bool managed = true);
        static ScamKeyword * makeInstance(std::string const & value,
                                          bool managed = true);
    };
}

#endif
