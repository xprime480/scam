#if ! defined(CALLCONT_HPP)
#define CALLCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class CallCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        CallCont(Continuation * cont, Env * env, ScamEngine * engine);

        static CallCont *
        makeInstance(Continuation * cont, Env * env, ScamEngine * engine);

    public:
        void mark() const override;
        void handleValue(ScamValue value) override;

    private:
        Continuation * cont;
        Env          * env;
    };
}

#endif
