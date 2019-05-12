#if ! defined(SCAMREAL_HPP)
#define SCAMREAL_HPP 1

#include "expr/ScamComplex.hpp"

#include <string>

namespace scam
{
    class ScamReal : public ScamComplex
    {
    private:
        friend class MemoryManager;

    protected:
        ScamReal(double value, bool exact, bool managed = true);

    private:
        static ScamReal *
        makeInstance(double value, bool exact, bool managed = true);
    public:
        std::string toString() const override;

        bool isReal() const override;
        double toReal() const override;

    private:
        double const value;
    };
}

#endif
