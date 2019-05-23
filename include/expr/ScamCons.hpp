#if ! defined(SCAMCONS_H)
#define SCAMCONS_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamCons : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamCons(ScamValue car, ScamValue cdr);
        static ScamCons * makeInstance(ScamValue car, ScamValue cdr);

    public:
        void eval(Continuation * cont, Env * env) const override;
        void mapEval(Continuation * cont, Env * env) const override;

        ScamValue getCar() const override;
        ScamValue getCdr() const override;

        size_t length() const override;
        ScamValue nthcar(size_t n) const override;
        ScamValue nthcdr(size_t n) const override;

        bool equals(ConstScamValue expr) const override;
    };
}

#endif
