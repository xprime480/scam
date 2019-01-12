#if ! defined(PRIMITIVE_PREDICATES_H)
#define PRIMITIVE_PREDICATES_H 1

#include "prim/Primitive.hpp"

namespace scam
{
#if defined(DECL_PREDICATE)
#error "DECL_PREDICATE should not be defined"
#else

#define DECL_PREDICATE(name) \
    class name : public Primitive \
    { \
    public: \
        name(); \
        void applyArgs(ScamExpr * args, ContHandle cont) override; \
    } \

    DECL_PREDICATE(NilP);
    DECL_PREDICATE(ErrorP);
    DECL_PREDICATE(ConsP);
    DECL_PREDICATE(ListP);
    DECL_PREDICATE(VectorP);
    DECL_PREDICATE(BoolP);
    DECL_PREDICATE(CharP);
    DECL_PREDICATE(StringP);
    DECL_PREDICATE(SymbolP);
    DECL_PREDICATE(KeywordP);
    DECL_PREDICATE(NumericP);
    DECL_PREDICATE(FloatP);
    DECL_PREDICATE(IntegerP);
    DECL_PREDICATE(ProcP);
    DECL_PREDICATE(ClassP);
    DECL_PREDICATE(InstanceP);

#undef DECL_PREDICATE
#endif

}

#endif
