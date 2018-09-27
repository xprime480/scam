#if ! defined(CONTINUATION_H)
#define CONTINUATION_H

#include <memory>

namespace scam
{
    class ScamExpr;

    class Continuation
    {
    public:
        virtual ~Continuation() {};

        virtual void run(std::shared_ptr<ScamExpr> expr) const = 0;
    };
}

#endif
