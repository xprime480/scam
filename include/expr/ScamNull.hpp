#if ! defined(SCAMNULL_H)
#define SCAMNULL_H 1

#include "expr/ScamExpr.hpp"
#include "expr/ScamData.hpp"

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
        void eval(Continuation * cont, Env * env) const override;

        bool equals(ConstExprHandle expr) const override;
    };
}

#endif
