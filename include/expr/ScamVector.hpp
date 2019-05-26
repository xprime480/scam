#if ! defined(SCAMVECTOR_H)
#define SCAMVECTOR_H 1

#include "expr/ScamData.hpp"

#include <vector>

namespace scam
{
    using ExprVec = std::vector<ScamValue>;

    class ScamVector : public ScamData
    {
    private:
        friend class MemoryManager;
        ScamVector(ExprVec const & elts);
        static ScamVector * makeInstance(ExprVec const & elts);
    };
}

#endif
