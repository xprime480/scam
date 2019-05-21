#if ! defined(SCAMSYMBOL_H)
#define SCAMSYMBOL_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamSymbol : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamSymbol(std::string const & value, bool managed = true);
        static ScamSymbol * makeInstance(std::string const & value,
                                         bool managed = true);

    public:
        std::string toString() const override;
        void eval(Continuation * cont, Env * env) const override;

        bool equals(ConstExprHandle expr) const override;
    };
}

#endif
