#if ! defined(SCAMCLASS_H)
#define SCAMCLASS_H 1

#include "expr/ScamExpr.hpp"

namespace scam
{
    class ScamClassAdapter;
    class Env;

    class ScamClass : public ScamExpr
    {
    private:
        friend class MemoryManager;
        ScamClass(ScamExpr * base,
                  ScamExpr * vars,
                  ScamExpr * funs,
                  Env * capture);
        static ScamClass * makeInstance(ScamExpr * base,
                                        ScamExpr * vars,
                                        ScamExpr * funs,
                                        Env * capture);

    public:
        void mark() const override;

        std::string toString() const override;

        bool hasApply() const override;
        void apply(ScamExpr * args, Continuation * cont, Env * env) override;

        bool isProcedure() const override;
        bool isClass() const override;

        friend class ScamClassAdapter;

    private:
        ScamExpr * base;
        ScamExpr * vars;
        ScamExpr * funs;
        Env *      capture;
    };
}

#endif
