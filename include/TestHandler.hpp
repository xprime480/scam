#if ! defined(TESTHANDLER_HPP)
#define TESTHANDLER_HPP 1

#include "Handler.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class TestHandler : public Handler
    {
    public:
        TestHandler(Continuation * cont);
        ScamValue handleError(ScamValue err) override;

    private:
        Continuation * cont;
    };
}

#endif

