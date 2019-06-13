#if ! defined(HANDLER_HPP)
#define HANDLER_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    class Handler
    {
    public:
        virtual ~Handler() {}

        virtual ScamValue handleError(ScamValue err);
    };
}

#endif
