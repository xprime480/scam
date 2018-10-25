#if ! defined(PRIMITIVE_MATHOPS_H)
#define PRIMITIVE_MATHOPS_H 1

#include "prim/Primitive.hpp"

#include "util/ArgListHelper.hpp"

namespace scam
{
    struct MathOpDef
    {
        char const * name;
        NumericalAlgorithm algo;
    };

    class MathOp : public Primitive
    {
    public:
        MathOp(MathOpDef const & def);

        void applyArgs(ScamExpr * args, ContHandle cont) override;

    private:
        NumericalAlgorithm algo;
    };

#define MATH_OP_DECL(Name) \
    class Name : public MathOp \
    { \
    public: \
        Name(); \
    };

    MATH_OP_DECL(Add);
    MATH_OP_DECL(Sub);
    MATH_OP_DECL(Mul);
    MATH_OP_DECL(Div);

#undef MATH_OP_DECL

}

#endif