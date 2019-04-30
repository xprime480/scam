#if ! defined(ENVHELPERCONT_HPP)
#define ENVHELPERCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class EnvHelperCont : public Continuation
    {
    protected:
        EnvHelperCont(ExprHandle sym,
                      Continuation * cont,
                      Env * env,
                      char const * name);

        void mark() const override;

        void run(ExprHandle expr) override;

    protected:
        ExprHandle sym;
        mutable Env * env;

        virtual void finish(ExprHandle expr) const = 0;

    private:
        Continuation * cont;
    };
}

#endif
