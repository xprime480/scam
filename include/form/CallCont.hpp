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

        CallCont(Continuation * cont, Env * env);
        static CallCont * makeInstance(Continuation * cont, Env * env);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        Continuation * cont;
        Env          * env;
    };
}

#endif
