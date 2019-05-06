#if ! defined(PRIMITIVE_EQUALP_H)
#define PRIMITIVE_EQUALP_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class ListParser;
    
    class EqualP : public Primitive
    {
    private:
        EqualP();

    public:
        static EqualP * makeInstance();
        void applyArgs(ExprHandle args, Continuation * cont) override;

        bool equals(ConstExprHandle expr) const override;
    };
}

#endif
