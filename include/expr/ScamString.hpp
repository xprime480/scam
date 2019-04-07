#if ! defined(SCAMSTRING_H)
#define SCAMSTRING_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamString : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamString(std::string const & value);
        static ScamString * makeInstance(std::string const & value);

    public:
        std::string toString() const override;

        bool isString() const override;

        bool equals(ScamExpr const * expr) const override;

    private:
        std::string const value;
    };
}

#endif
