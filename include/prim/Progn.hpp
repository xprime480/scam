#if ! defined(PRIMITIVE_PROGN_H)
#define PRIMITIVE_PROGN_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Progn : public Primitive
    {
    private:
        Progn();

    public:
        static Progn * makeInstance();
        void applyArgs(ExprHandle args, Continuation * cont) override;
    };
}

#endif
