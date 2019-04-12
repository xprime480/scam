#if ! defined(ENVHELPERCONT_HPP)
#define ENVHELPERCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class ScamExpr;
    class Continuation;
    class Env;

    class EnvHelperCont : public Continuation
    {
    protected:
        EnvHelperCont(ScamExpr * sym,
                      Continuation * cont,
                      Env * env,
                      char const * name);

        void mark() const override;

        void run(ScamExpr * expr) override;

    protected:
        ScamExpr * sym;
        mutable Env * env;

        virtual void finish(ScamExpr * expr) const = 0;

    private:
        Continuation * cont;
    };

}

#endif
