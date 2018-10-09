#if ! defined(SCAMVECTOR_H)
#define SCAMVECTOR_H 1

#include "expr/ScamExpr.hpp"

#include <vector>

namespace scam
{
    using ExprVec = std::vector<ExprHandle>;

    class ScamVector : public ScamExpr
    {
    public:
        ScamVector(ExprVec const & elts);

        std::string toString() const override;

        void eval(ContHandle cont, Env & env);

        bool isVector() const override;

        size_t length() const override;
        ExprHandle nth(size_t n) const override;

        ExprHandle clone() const override;

    private:
        ExprVec elts;
    };
}

#endif
