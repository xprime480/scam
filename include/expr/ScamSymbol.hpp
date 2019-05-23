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
        void eval(Continuation * cont, Env * env) const override;

        bool equals(ConstScamValue expr) const override;
    };
}

#endif
