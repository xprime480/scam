#if ! defined(SCAMVECTOR_H)
#define SCAMVECTOR_H 1

#include "expr/ScamExpr.hpp"

#include <vector>

namespace scam
{
    using ExprVec = std::vector<ExprHandle>;

    class ScamVector : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamVector(ExprVec const & elts);
        static ScamVector * makeInstance(ExprVec const & elts);

    public:
        size_t length() const override;
        ExprHandle nthcar(size_t n) const override;

        bool equals(ConstExprHandle expr) const override;
    };
}

#endif
