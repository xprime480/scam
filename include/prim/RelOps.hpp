#if ! defined(PRIMITIVE_COMPAREOPS_H)
#define PRIMITIVE_COMPAREOPS_H 1

#include "ScamFwd.hpp"

namespace scam
{
#define CMP_OP_DECL(Name)                                             \
    extern void apply##Name(ScamValue args, Continuation * cont);

    CMP_OP_DECL(Eq);
    CMP_OP_DECL(Ne);
    CMP_OP_DECL(Lt);
    CMP_OP_DECL(Le);
    CMP_OP_DECL(Gt);
    CMP_OP_DECL(Ge);

#undef CMP_OP_DECL
}

#endif
