#if ! defined(SCAMNIL_H)
#define SCAMNIL_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamNil : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamNil();
        static ScamNil * makeInstance();

    public:
        bool isManaged() const override;

        std::string toString() const override;

        bool isNil() const override;
        bool isList() const override;
        size_t length() const override;

        bool equals(ScamExpr const * expr) const override;
    };
}

#endif
