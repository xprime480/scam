#if ! defined(TEST_ACCUMULATOR_HPP)
#define TEST_ACCUMULATOR_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"
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
        void handleValue(ScamValue expr) override
        {
            Continuation::handleValue(expr);
            s << writeValue(expr) << "\n";
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
