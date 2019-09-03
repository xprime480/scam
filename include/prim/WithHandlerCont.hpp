#if ! defined(WITHHANDLERCONT_HPP)
#define WITHHANDLERCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class WithHandlerCont : public Continuation
    {
        friend class scam::MemoryManager;

        WithHandlerCont(Continuation * cont);
        static WithHandlerCont * makeInstance(Continuation * cont);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        Continuation * cont;
    };
}

#endif
