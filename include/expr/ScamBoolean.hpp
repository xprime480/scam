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
        bool isManaged() const override;

        std::string toString() const override;

        bool truth() const override;

        bool isBoolean() const override;

        bool equals(ScamExpr const * expr) const override;

    private:
        bool value;
    };
}

#endif
