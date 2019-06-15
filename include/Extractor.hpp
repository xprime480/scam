#if ! defined(TEST_EXTRACTOR_HPP)
#define TEST_EXTRACTOR_HPP 1

#include "Continuation.hpp"

#include "expr/ScamData.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/TypePredicates.hpp"

#include <iostream>
#include <memory>

namespace scam
{
    class Extractor : public Continuation
    {
    private:
        friend class MemoryManager;
        Extractor(ScamEngine * engine)
            : Continuation("Extractor", engine)
            , lastValue(makeNothing())
        {
        }

        static Extractor * makeInstance(ScamEngine * engine)
        {
            return new Extractor(engine);
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                Continuation::mark();
                lastValue->mark();
            }
        }

        void handleValue(ScamValue expr) override
        {
#if 0
            std::cerr << "Extractor " << id()
                      << " getting " << writeValue(expr) << "\n";
#endif
            Continuation::handleValue(expr);
            if ( ! isNothing(expr) ) {
                lastValue = expr;
            }
        }

        ScamValue getLastValue() const
        {
            return lastValue;
        }

    private:
        ScamValue lastValue;
    };
}

#endif
