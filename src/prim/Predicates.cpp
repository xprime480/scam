#include "prim/Predicates.hpp"

#include "Continuation.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

namespace
{
    extern bool isScamNaN(ScamValue data);
    extern bool isScamInfinite(ScamValue data);
    extern bool isScamFinite(ScamValue data);
}

#if defined(DEFINE_PREDICATE)
#error "DEFINE_PREDICATE already defined"
#else

#define DEFINE_PREDICATE(cls, label, pred)                         \
        void scam::apply##cls(ScamValue args, Continuation * cont) \
        {                                                          \
            static const char * name { label };                    \
            ObjectParameter p0;                                    \
            if ( argsToParms(args, name, p0) ) {                   \
                bool answer = pred(p0.value);                      \
                cont->handleValue(makeBoolean(answer));            \
            }                                                      \
        }

DEFINE_PREDICATE(NullP, "null?", isNull)
DEFINE_PREDICATE(ErrorP, "error-object?", isError)
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
DEFINE_PREDICATE(InexactP, "inexact?", isInexact)
DEFINE_PREDICATE(NanP, "nan?", isScamNaN);
DEFINE_PREDICATE(InfiniteP, "infinite?", isScamInfinite);
DEFINE_PREDICATE(FiniteP, "finite?", isScamFinite);
DEFINE_PREDICATE(ProcedureP, "procedure?", isScamProcedure)
DEFINE_PREDICATE(ClassP, "class?", isClass)
DEFINE_PREDICATE(InstanceP, "instance?", isInstance)
DEFINE_PREDICATE(DictP, "dict?", isDict)
DEFINE_PREDICATE(PortP, "port?", isPort)
DEFINE_PREDICATE(EofP, "eof-object?", isEof)
DEFINE_PREDICATE(EnvironmentP, "environment?", isEnv)

#endif

namespace
{
    bool isScamNaN(ScamValue data)
    {
        if ( ! isNumeric(data) ) {
            return false;
        }
        if ( isNaN(data) ) {
            return true;
        }
        if ( isPureComplex(data) ) {
            return isNaN(data->realPart()) || isNaN(data->imagPart());
        }
        return false;
    }

    bool isScamInfinite(ScamValue data)
    {
        if ( ! isNumeric(data) ) {
            return false;
        }
        if ( isPosInf(data) || isNegInf(data)  ) {
            return true;
        }
        if ( isPureComplex(data) ) {
            return (isScamInfinite(data->realPart()) ||
                    isScamInfinite(data->imagPart()));
        }
        return false;
    }

    bool isScamFinite(ScamValue data)
    {
        if ( ! isNumeric(data) ) {
            return false;
        }
        return ! ( isScamNaN(data) || isScamInfinite(data) );
    }
}



