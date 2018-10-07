#if ! defined(CONTINUATION_H)
#define CONTINUATION_H

#include <memory>

namespace scam
{
    class ScamExpr;
    class Continuation;

    using ExprHandle = std::shared_ptr<ScamExpr>;
    using ContHandle = std::shared_ptr<Continuation> ;

    class Continuation
    {
    public:
        virtual ~Continuation() {};

        virtual void run(ExprHandle expr) const = 0;
    };
}

#endif
