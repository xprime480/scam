#if ! defined(SCAMCHARACTER_H)
#define SCAMCHARACTER_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamCharacter : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamCharacter(std::string const & value);
        static ScamCharacter * makeInstance(std::string const & value);

    public:
        std::string toString() const override;

        bool isChar() const override;
        char toChar() const override;

        bool equals(ScamExpr const * expr) const override;

    private:
        std::string const value;
    };
}

#endif
