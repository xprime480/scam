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
                      char const * name);

        void mark() override;
        void handleValue(ScamValue value) override;

    protected:
        ScamValue     sym;
        mutable Env * env;

        virtual ScamValue finish(ScamValue expr) const = 0;

    private:
        Continuation * cont;
    };
}

#endif
