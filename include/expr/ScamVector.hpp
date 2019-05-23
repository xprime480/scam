#if ! defined(SCAMVECTOR_H)
#define SCAMVECTOR_H 1

#include "expr/ScamExpr.hpp"

#include <vector>

namespace scam
{
    using ExprVec = std::vector<ScamValue>;

    class ScamVector : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamVector(ExprVec const & elts);
        static ScamVector * makeInstance(ExprVec const & elts);

    public:
        size_t length() const override;
        ScamValue nthcar(size_t n) const override;

        bool equals(ConstScamValue expr) const override;
    };
}

#endif
