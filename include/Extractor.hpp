#if ! defined(TEST_EXTRACTOR_HPP)
#define TEST_EXTRACTOR_HPP 1

#include "Continuation.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

#include <memory>

namespace scam
{
    class Extractor : public Continuation
    {
    public:
        Extractor()
            : Continuation("Extractor")
            , e(ExpressionFactory::makeNull())
        {
        }

        void run(ScamExpr * expr) override
        {
            Continuation::run(expr);
            e = expr->clone();
        }

        ExprHandle getExpr() const
        {
            return e;
        }

    private:
        ExprHandle e;
    };
}

#endif
