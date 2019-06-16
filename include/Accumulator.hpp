#if ! defined(TEST_ACCUMULATOR_HPP)
#define TEST_ACCUMULATOR_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"

#include <sstream>

namespace scam
{
    class Accumulator : public Continuation
    {
    private:
        friend class MemoryManager;
        Accumulator(ScamEngine * engine)
            : Continuation("Accumulator", engine)
        {
        }

        static Accumulator * makeInstance(ScamEngine * engine)
        {
            return new Accumulator(engine);
        }

    public:
        void handleValue(ScamValue value) override
        {
            Continuation::handleValue(value);
            if ( isNothing(value) ) {
                return;
            }
            s << writeValue(value) << "\n";
        }

        std::string getResult() const
        {
            return s.str();
        }

    private:
        std::stringstream s;
    };
}

#endif
