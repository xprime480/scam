#if ! defined(PRIMITIVE_PREDICATES_H)
#define PRIMITIVE_PREDICATES_H 1

#include "ScamFwd.hpp"

namespace scam
{
#if defined(DECL_PREDICATE)
#error "DECL_PREDICATE should not be defined"
#else

#define DECL_PREDICATE(name)                                            \
    void apply##name(ScamValue args, Continuation * cont);

    DECL_PREDICATE(NullP);
    DECL_PREDICATE(ErrorP);
    DECL_PREDICATE(PairP);
    DECL_PREDICATE(ListP);
    DECL_PREDICATE(VectorP);
    DECL_PREDICATE(BoolP);
    DECL_PREDICATE(CharP);
    DECL_PREDICATE(StringP);
    DECL_PREDICATE(SymbolP);
    DECL_PREDICATE(KeywordP);
    DECL_PREDICATE(NumericP);
    DECL_PREDICATE(ComplexP);
    DECL_PREDICATE(RealP);
    DECL_PREDICATE(RationalP);
    DECL_PREDICATE(IntegerP);
    DECL_PREDICATE(ExactP);
    DECL_PREDICATE(InexactP);
    DECL_PREDICATE(NanP);
    DECL_PREDICATE(InfiniteP);
    DECL_PREDICATE(FiniteP);
    DECL_PREDICATE(ProcedureP);
    DECL_PREDICATE(ClassP);
    DECL_PREDICATE(InstanceP);
    DECL_PREDICATE(DictP);
    DECL_PREDICATE(PortP);
    DECL_PREDICATE(EofP);
    DECL_PREDICATE(EnvironmentP);

#undef DECL_PREDICATE
#endif
}

#endif
