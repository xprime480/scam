#if ! defined(TEST_EXTRACTOR_HPP)
#define TEST_EXTRACTOR_HPP 1

#include "Continuation.hpp"

#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

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
            , lastValue(makeNothing())
        {
        }

        static Extractor * makeInstance()
        {
            return new Extractor;
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
