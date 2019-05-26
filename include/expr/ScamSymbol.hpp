#if ! defined(SCAMSYMBOL_H)
#define SCAMSYMBOL_H 1

#include "expr/ScamData.hpp"

namespace scam
{
    class ScamSymbol : public ScamData
    {
    private:
        friend class MemoryManager;
        ScamSymbol(std::string const & value, bool managed = true);
        static ScamSymbol * makeInstance(std::string const & value,
                                         bool managed = true);
    };
}

#endif
