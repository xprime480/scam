#if ! defined(SCAMBOOLEAN_H)
#define SCAMBOOLEAN_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamBoolean : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamBoolean(bool value);
        static ScamBoolean * makeInstance(bool value);

    public:
        std::string toString() const override;

        bool truth() const override;

        bool isBoolean() const override;

        bool equals(ConstExprHandle expr) const override;

    private:
        bool value;
    };
}

#endif
