#if ! defined(SCAMCONS_H)
#define SCAMCONS_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamCons : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamCons(ExprHandle car, ExprHandle cdr);
        static ScamCons * makeInstance(ExprHandle car, ExprHandle cdr);

    public:
        void mark() const override;

        std::string toString() const override;
        void eval(Continuation * cont, Env * env) const override;
        void mapEval(Continuation * cont, Env * env) const override;

        bool isCons() const override;
        bool isList() const override;

        ExprHandle getCar() const override;
        ExprHandle getCdr() const override;

        size_t length() const override;
        ExprHandle nthcar(size_t n) const override;
        ExprHandle nthcdr(size_t n) const override;

        bool equals(ConstExprHandle expr) const override;

    private:
        ExprHandle car;
        ExprHandle cdr;
    };
}

#endif
