#if ! defined(ENVHELPERCONT_HPP)
#define ENVHELPERCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class EnvHelperCont : public Continuation
    {
    protected:
        EnvHelperCont(ScamValue sym,
                      Continuation * cont,
                      Env * env,
                      ScamEngine * engine,
                      char const * name);

        void mark() const override;

        void run(ScamValue expr) override;

    protected:
        ScamValue     sym;
        mutable Env * env;

        virtual void finish(ScamValue expr) const = 0;

    private:
        Continuation * cont;
    };
}

#endif
