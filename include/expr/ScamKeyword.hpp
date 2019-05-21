#if ! defined(SCAMKEYWORD_H)
#define SCAMKEYWORD_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamKeyword : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamKeyword(std::string const & value, bool managed = true);
        static ScamKeyword * makeInstance(std::string const & value,
                                          bool managed = true);

    public:
        std::string toString() const override;

        bool equals(ConstExprHandle expr) const override;
    };
}

#endif
