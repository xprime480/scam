#if ! defined(SCAMINTEGER_H)
#define SCAMINTEGER_H 1

#include "expr/ScamRational.hpp"

#include <string>

namespace scam
{
    class ScamInteger : public ScamRational
    {
    private:
        friend class MemoryManager;
        ScamInteger(int value);
        static ScamInteger * makeInstance(int value);

    public:
        std::string toString() const override;

        bool isInteger() const override;
        int toInteger() const override;

    private:
        int const value;
    };
}

#endif
