#if ! defined(UNIFY_HPP)
#define UNIFY_HPP 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Unify : public Primitive
    {
    private:
        Unify();

    public:
        static Unify * makeInstance();
        void applyArgs(ExprHandle args, Continuation * cont) override;
    };
}

#endif
