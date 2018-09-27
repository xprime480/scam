#if ! defined(SCAMCONTEXT_H)
#define SCAMCONTEXT_H 1

#include "Continuation.hpp"

#include <memory>

namespace scam
{
    struct ScamContext
    {
        std::shared_ptr<Continuation> cont;
    };
}

#endif
