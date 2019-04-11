#if ! defined(BINDER_H)
#define BINDER_H

namespace scam
{
    class ScamExpr;
    class Env;

    class Binder
    {
    public:
        Binder(Env * capture);

        Env * bind(ScamExpr * formals, ScamExpr * actuals) const;

    private:
        Env * capture;

        void bindOne(Env * env, ScamExpr * syms, ScamExpr * vals) const;
    };
}

#endif
