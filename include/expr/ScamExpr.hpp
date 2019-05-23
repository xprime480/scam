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
        apply(ExprHandle args, Continuation * cont, Env * env);

        virtual void mapEval(Continuation * cont, Env * env) const;

        virtual char toChar() const;
        double asDouble() const;
        std::pair<int, int> asRational() const;
        int asInteger() const;

        ConstExprHandle realPart() const;
        ConstExprHandle imagPart() const;

        virtual ExprHandle getCar() const;
        virtual ExprHandle getCdr() const;


        virtual size_t length() const;
        virtual ExprHandle nthcar(size_t n) const;
        virtual ExprHandle nthcdr(size_t n) const;

        virtual ExprHandle withEnvUpdate(Env * updated) const;

        virtual void setSelf(ExprHandle expr) const;
        virtual void setParent(ExprHandle expr) const;

        virtual bool equals(ConstExprHandle expr) const;
    };
}

#endif
