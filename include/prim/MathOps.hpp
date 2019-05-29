#if ! defined(PRIMITIVE_MATHOPS_H)
#define PRIMITIVE_MATHOPS_H 1

#include "ScamFwd.hpp"

namespace scam
{

#define MATH_OP_DECL(Name) \
        extern void apply##Name(ScamValue args,               \
                                Continuation * cont,          \
                                ScamEngine * engine);

    MATH_OP_DECL(Add);
    MATH_OP_DECL(Sub);
    MATH_OP_DECL(Mul);
    MATH_OP_DECL(Div);
    MATH_OP_DECL(Mod);

#undef MATH_OP_DECL
}

#endif
