#if ! defined(UNDEFINECONT_HPP)
#define UNDEFINECONT_HPP 1

#include "form/EnvHelperCont.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class UndefineCont : public EnvHelperCont
    {
    private:
        friend class scam::MemoryManager;

        UndefineCont(ScamValue sym, Continuation * cont, Env * env);

        static UndefineCont *
        makeInstance(ScamValue sym, Continuation * cont, Env * env);

    protected:
        void finish(ScamValue expr) const override;
    };
}

#endif
