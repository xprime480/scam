#if ! defined(REPLHANDLER_HPP)
#define REPLHANDLER_HPP 1

#include "Handler.hpp"

namespace scam
{
    class ReplHandler : public Handler
    {
    public:
        ScamValue handleError(ScamValue err) override;
    };
}

#endif
