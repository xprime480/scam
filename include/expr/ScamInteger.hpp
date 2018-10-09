#if ! defined(SCAMINTEGER_H)
#define SCAMINTEGER_H 1

#include "expr/ScamFloat.hpp"

#include <string>

namespace scam
{
    class ScamInteger : public ScamFloat
    {
    public:
        ScamInteger(int value);
        std::string toString() const override;

        bool isInteger() const override;
        int toInteger() const override;

        ExprHandle clone() const override;

    private:
        int const value;
    };
}

#endif
