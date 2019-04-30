#if ! defined(SCAMCHARACTER_H)
#define SCAMCHARACTER_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamCharacter : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamCharacter(const std::string & value);
        static ScamCharacter * makeInstance(const std::string & value);

    public:
        std::string toString() const override;

        bool isChar() const override;
        char toChar() const override;

        bool equals(ConstExprHandle expr) const override;

    private:
        const char value;
    };
}

#endif
