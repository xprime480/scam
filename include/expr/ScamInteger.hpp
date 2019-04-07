#if ! defined(SCAMINTEGER_H)
#define SCAMINTEGER_H 1

#include "expr/ScamFloat.hpp"

#include <string>

namespace scam
{
    class ScamInteger : public ScamFloat
    {
    private:
        friend class MemoryManager;
        ScamInteger(int value);
        static ScamInteger * makeInstance(int value);

    public:
        std::string toString() const override;

        bool isInteger() const override;
        int toInteger() const override;

        bool equals(ScamExpr const * expr) const override;

    private:
        int const value;
    };
}

#endif
