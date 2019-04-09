#if ! defined(PRIMITIVE_MISC_H)
#define PRIMITIVE_MISC_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Progn : public Primitive
    {
    private:
        Progn();

    public:
        static Progn * makeInstance();
        void applyArgs(ScamExpr * args, Continuation * cont) override;
    };
}

#endif
