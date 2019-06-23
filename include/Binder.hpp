#if ! defined(BINDER_H)
#define BINDER_H

#include "ScamFwd.hpp"

namespace scam
{
    class Binder
    {
    public:
        Binder(Env * capture);

        Env * bind(ScamValue formals, ScamValue rest, ScamValue actuals) const;

    private:
        Env * capture;

        void bindOne(Env * env,
                     ScamValue syms,
                     ScamValue rest,
                     ScamValue vals) const;
    };
}

#endif
