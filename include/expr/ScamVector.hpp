#if ! defined(SCAMVECTOR_H)
#define SCAMVECTOR_H 1

#include "expr/ScamExpr.hpp"

#include <vector>

namespace scam
{
    using ExprVec = std::vector<ScamExpr *>;

    class ScamVector : public ScamExpr
    {
    private:
        ScamVector(ExprVec const & elts);

    public:
        static ScamVector * makeInstance(ExprVec const & elts);
        void mark() const override;

        std::string toString() const override;

        void eval(ContHandle cont, Env env);

        bool isVector() const override;

        size_t length() const override;
        ScamExpr * nthcar(size_t n) const override;

        bool equals(ScamExpr const * expr) const override;

    private:
        ExprVec elts;
    };
}

#endif
