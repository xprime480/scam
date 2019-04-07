#if ! defined(SCAMCONS_H)
#define SCAMCONS_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamCons : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamCons(ScamExpr * car, ScamExpr * cdr);
        static ScamCons * makeInstance(ScamExpr * car, ScamExpr * cdr);

    public:
        void mark() const override;

        std::string toString() const override;
        void eval(ContHandle cont, Env env) override;
        void mapEval(ContHandle cont, Env env) override;

        bool isCons() const override;
        bool isList() const override;

        ScamExpr * getCar() const override;
        ScamExpr * getCdr() const override;

        size_t length() const override;
        ScamExpr * nthcar(size_t n) const override;
        ScamExpr * nthcdr(size_t n) const override;

        bool equals(ScamExpr const * expr) const override;

    private:
        ScamExpr * car;
        ScamExpr * cdr;
    };
}

#endif
