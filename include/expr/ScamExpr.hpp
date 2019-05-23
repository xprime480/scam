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

        bool hasApply() const;

        virtual void
        apply(ScamValue args, Continuation * cont, Env * env);

        virtual void mapEval(Continuation * cont, Env * env) const;

        ConstScamValue realPart() const;
        ConstScamValue imagPart() const;

        virtual ScamValue getCar() const;
        virtual ScamValue getCdr() const;

        virtual size_t length() const;
        virtual ScamValue nthcar(size_t n) const;
        virtual ScamValue nthcdr(size_t n) const;

        virtual ScamValue withEnvUpdate(Env * updated) const;

        virtual void setSelf(ScamValue expr) const;
        virtual void setParent(ScamValue expr) const;

        virtual bool equals(ConstScamValue expr) const;
    };
}

#endif
