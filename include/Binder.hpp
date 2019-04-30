#if ! defined(BINDER_H)
#define BINDER_H

#include "ScamFwd.hpp"

namespace scam
{
    class Binder
    {
    public:
        Binder(Env * capture);

        Env * bind(ExprHandle formals, ExprHandle actuals) const;

    private:
        Env * capture;

        void bindOne(Env * env, ExprHandle syms, ExprHandle vals) const;
    };
}

#endif
