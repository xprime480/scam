#if ! defined(TEST_EXTRACTOR_HPP)
#define TEST_EXTRACTOR_HPP 1

#include "Continuation.hpp"

#include "expr/ScamData.hpp"
#include "expr/ValueFactory.hpp"

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
            , e(makeNothing())
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

        void run(ScamValue expr) override
        {
#if 0
            std::cerr << "Extractor " << id()
                      << " getting " << writeValue(expr) << "\n";
#endif
            Continuation::run(expr);
            e = expr;
        }

        ScamValue getExpr() const
        {
            return e;
        }

    private:
        ScamValue e;
    };
}

#endif
