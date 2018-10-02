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
        void run(std::shared_ptr<ScamExpr> e) const override
        {
            expr = e;
        }

        std::shared_ptr<ScamExpr> getExpr() const
        {
            return expr;
        }

    private:
        mutable std::shared_ptr<ScamExpr> expr;
    };
}

#endif
