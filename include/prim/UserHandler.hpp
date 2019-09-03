#if ! defined(USERHANDLER_HPP)
#define USERHANDLER_HPP 1

#include "Handler.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class UserHandler : public Handler
    {
    private:
        friend class MemoryManager;

        UserHandler(ScamValue handler, Continuation * cont, Env * env);

        static UserHandler *
        makeInstance(ScamValue handler, Continuation * cont, Env * env);

    public:
        void mark() override;

        ScamValue handleError(ScamValue err) override;

    private:
        ScamValue      handler;
        Continuation * cont;
        Env          * env;
    };
}

#endif
