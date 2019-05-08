#if ! defined(SCAMCOMPLEX_HPP)
#define SCAMCOMPLEX_HPP 1

#include "expr/ScamNumeric.hpp"

#include <string>

namespace scam
{
    class ScamComplex : public ScamNumeric
    {
    private:
        friend class MemoryManager;

    protected:
        ScamComplex(double real, double imag, bool exact);

    private:
        static ScamComplex * makeInstance(double real, double imag, bool exact);

    public:
        std::string toString() const override;

        bool isComplex() const override;

    protected:
        double realPart() const override;
        double imagPart() const override;

    private:
        double const real;
        double const imag;
    };
}

#endif
