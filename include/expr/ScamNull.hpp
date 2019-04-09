#if ! defined(SCAMNULL_H)
#define SCAMNULL_H 1

#include "expr/ScamExpr.hpp"

#include <string>

namespace scam
{
    class ScamNull : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamNull();
        static ScamNull * makeInstance();

    public:
        std::string toString() const override;
        void eval(Continuation * cont, Env env) override;

        bool isNull() const override;
        bool truth() const override;

        bool equals(ScamExpr const * expr) const override;
    };
}

#endif
