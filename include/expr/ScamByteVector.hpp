#if ! defined(SCAMBYTEVECTOR_H)
#define SCAMBYTEVECTOR_H 1

#include "expr/ScamExpr.hpp"

#include <vector>

namespace scam
{
    using ByteVec = std::vector<unsigned char>;

    class ScamByteVector : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamByteVector(ByteVec const & elts);
        static ScamByteVector * makeInstance(ByteVec const & elts);

    public:
        ~ScamByteVector();

        std::string toString() const override;

        size_t length() const override;
        ExprHandle nthcar(size_t n) const override;

        bool equals(ConstExprHandle expr) const override;
    };
}

#endif
