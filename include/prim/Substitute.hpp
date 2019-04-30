#if ! defined(SUBSTITUTE_HPP)
#define SUBSTITUTE_HPP 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Substitute : public Primitive
    {
    private:
        Substitute();

    public:
        static Substitute * makeInstance();
        void applyArgs(ExprHandle args, Continuation * cont) override;
    };
}

#endif
