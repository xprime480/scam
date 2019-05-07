#if ! defined(SCAMREAL_HPP)
#define SCAMREAL_HPP 1

#include "expr/ScamNumeric.hpp"

#include <string>

namespace scam
{
    class ScamReal : public ScamNumeric
    {
    private:
        friend class MemoryManager;

    protected:
        ScamReal(double value);

    private:
        static ScamReal * makeInstance(double value);

    public:
        std::string toString() const override;

        bool isReal() const override;
        double toReal() const override;

        bool equals(ConstExprHandle expr) const override;

    private:
        double const value;
    };
}

#endif
