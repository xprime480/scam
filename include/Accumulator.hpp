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
        Accumulator()
            : Continuation("Accumulator")
        {
        }

        static Accumulator * makeInstance()
        {
            return new Accumulator();
        }

    public:
        void run(ScamValue expr) override
        {
            Continuation::run(expr);
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
