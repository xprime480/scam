#if ! defined(SCAMEXPR_HPP)
#define SCAMEXPR_HPP 1

#include "expr/ScamData.hpp"

#include "ScamFwd.hpp"

#include <string>

namespace scam
{
    class ScamExpr : public ScamData
    {
    protected:
        ScamExpr(unsigned long type, bool managed = true);

    public:
        virtual void eval(Continuation * cont, Env * env) const;

        virtual void
        apply(ScamValue args, Continuation * cont, Env * env);

        virtual void mapEval(Continuation * cont, Env * env) const;
    };
}

#endif
