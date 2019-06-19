#if ! defined(TESTHANDLER_HPP)
#define TESTHANDLER_HPP 1

#include "Handler.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class TestHandler : public Handler
    {
    private:
        friend class MemoryManager;
        TestHandler(Continuation * cont);
        static TestHandler * makeInstance(Continuation * cont);

    public:
        void mark() override;

        ScamValue handleError(ScamValue err) override;

    private:
        Continuation * cont;
    };
}

#endif
