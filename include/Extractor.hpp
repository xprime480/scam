#if ! defined(TEST_EXTRACTOR_HPP)
#define TEST_EXTRACTOR_HPP 1

#include "Continuation.hpp"

#include "expr/ScamExpr.hpp"

#include <memory>

namespace scam
{
    class Extractor : public Continuation
    {
    public:
        void run(ExprHandle e) const override
        {
            expr = e;
        }

        ExprHandle getExpr() const
        {
            return expr;
        }

    private:
        mutable ExprHandle expr;
    };
}

#endif
