#if ! defined(SCAMBYTEVECTOR_H)
#define SCAMBYTEVECTOR_H 1

#include "expr/ScamData.hpp"

#include <vector>

namespace scam
{
    using ByteVec = std::vector<unsigned char>;

    class ScamByteVector : public ScamData
    {
    private:
        friend class MemoryManager;
        ScamByteVector(const ByteVec & elts);
        static ScamByteVector * makeInstance(ByteVec const & elts);
    };
}

#endif
