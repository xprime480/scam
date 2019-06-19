#if ! defined(WITHHANDLERCONT_HPP)
#define WITHHANDLERCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class WithHandlerCont : public Continuation
    {
        friend class scam::MemoryManager;

        WithHandlerCont(Continuation * cont, ScamEngine * engine);

        static WithHandlerCont *
        makeInstance(Continuation * cont, ScamEngine * engine);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        Continuation * cont;
    };
}

#endif
