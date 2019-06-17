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
        void mark() override
        {
            if ( ! isMarked() ) {
                Continuation::mark();
                lastValue->mark();
            }
        }

        void handleValue(ScamValue value) override
        {
#if 0
            std::cerr << "Extractor " << id()
                      << " getting " << writeValue(value) << "\n";
#endif
            Continuation::handleValue(value);
            if ( ! isNothing(value) ) {
                lastValue = value;
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
