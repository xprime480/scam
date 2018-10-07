#if ! defined(PRIMITIVE_ADD_H)
#define PRIMITIVE_ADD_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Add : public Primitive
    {
    public:
        Add();

        std::shared_ptr<ScamExpr> clone();

        void applyArgs(std::shared_ptr<ScamExpr> const & args,
                       std::shared_ptr<Continuation> cont) override;
    };
}

#endif
