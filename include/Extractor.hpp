#if ! defined(TEST_EXTRACTOR_HPP)
#define TEST_EXTRACTOR_HPP 1

#include "Continuation.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"

#include <iostream>
#include <memory>

namespace scam
{
    class Extractor : public Continuation
    {
    private:
        friend class MemoryManager;
        Extractor()
            : Continuation("Extractor")
            , e(ExpressionFactory::makeNull())
        {
        }

        static Extractor * makeInstance()
        {
            return new Extractor();
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                Continuation::mark();
                e->mark();
            }
        }

        void run(ScamExpr * expr) override
        {
#if 0
            std::cerr << "Extractor " << id()
                      << " getting " << expr->toString() << "\n";
#endif
            Continuation::run(expr);
            e = expr;
        }

        ScamExpr * getExpr() const
        {
            return e;
        }

    private:
        ScamExpr * e;
    };
}

#endif
