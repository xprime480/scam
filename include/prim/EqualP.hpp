#if ! defined(PRIMITIVE_EQUALP_H)
#define PRIMITIVE_EQUALP_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class EqualP : public Primitive
    {
    private:
        EqualP();

    public:
        static EqualP * makeInstance();
        void applyArgs(ScamExpr * args, ContHandle cont) override;

        bool equals(ScamExpr const * expr) const override;
    };
}

#endif
