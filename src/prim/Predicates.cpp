#include "prim/Predicates.hpp"

#include "Continuation.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

#if defined(DEFINE_PREDICATE)
#error "DEFINE_PREDICATE already defined"
#else

#define DEFINE_PREDICATE(cls, label, pred)                      \
        void scam::apply##cls(ScamValue args,                   \
                        Continuation * cont,                    \
                        ScamEngine * engine)                    \
        {                                                       \
            static const char * name { label };                 \
            ArgListHelper helper(args);                         \
                                                                \
            ScamValue arg;                                      \
            if ( ! wantObject(name, helper, cont, arg) ) {      \
                return;                                         \
            }                                                   \
            if ( ! finishArgs(name, helper, cont) ) {           \
                return;                                         \
            }                                                   \
            bool answer = pred(arg);                            \
            ScamValue rv = makeBoolean(answer);                 \
            cont->run(rv);                                      \
        }

DEFINE_PREDICATE(NullP, "null?", isNull)
DEFINE_PREDICATE(ErrorP, "error?", isError)
DEFINE_PREDICATE(PairP, "pair?", isPair)
DEFINE_PREDICATE(ListP, "list?", isList)
DEFINE_PREDICATE(VectorP, "vector?", isVector)
DEFINE_PREDICATE(BoolP, "boolean?", isBoolean)
DEFINE_PREDICATE(CharP, "char?", isChar)
DEFINE_PREDICATE(StringP, "string?", isString)
DEFINE_PREDICATE(SymbolP, "symbol?", isSymbol)
DEFINE_PREDICATE(KeywordP, "keyword?", isKeyword)
DEFINE_PREDICATE(NumericP, "numeric?", isNumeric)
DEFINE_PREDICATE(ComplexP, "complex?", isComplex)
DEFINE_PREDICATE(RealP, "real?", isReal)
DEFINE_PREDICATE(RationalP, "rational?", isRational)
DEFINE_PREDICATE(IntegerP, "integer?", isInteger)
DEFINE_PREDICATE(ExactP, "exact?", isExact)
DEFINE_PREDICATE(ProcP, "proc?", isProcedure)
DEFINE_PREDICATE(ClassP, "class?", isClass)
DEFINE_PREDICATE(InstanceP, "instance?", isInstance)
DEFINE_PREDICATE(DictP, "dict?", isDict)

#endif
