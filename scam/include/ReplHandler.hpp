#if ! defined(REPLHANDLER_HPP)
#define REPLHANDLER_HPP 1

#include "Handler.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ReplHandler : public Handler
    {
    private:
        friend class MemoryManager;
        ReplHandler();
        static ReplHandler * makeInstance();

    public:
        ScamValue handleError(ScamValue err) override;
    };
}

#endif
