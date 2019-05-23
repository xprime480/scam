#include "prim/Predicates.hpp"

#include "Continuation.hpp"
#include "ScamFwd.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/TypePredicates.hpp"
#include "input/SingletonParser.hpp"
#include "util/ArgListHelper.hpp"

#include <functional>

using namespace scam;
using namespace std;

namespace
{
    using ExprPredicate = function<bool(const ScamData *)>;

    extern void apply_predicate(const char * name,
                                ExprPredicate pred,
                                ScamValue args,
                                Continuation * cont);
}

#if defined(DEFINE_PREDICATE)
#error "DEFINE_PREDICATE already defined"
#else

#define DEFINE_PREDICATE(cls, label, pred)                      \
    cls::cls() : Primitive(label) {}                            \
    cls * cls::makeInstance()                                   \
    {                                                           \
        return new cls();                                       \
    }                                                           \
    void cls::applyArgs(ScamValue args, Continuation * cont)   \
    {                                                           \
        apply_predicate(label, pred, args, cont);               \
    }                                                           \
    bool cls::equals(ConstScamValue expr) const                \
    {                                                           \
        return ( expr && writeValue(expr) == label );           \
    }


DEFINE_PREDICATE(NilP, "nil?", &TypePredicates::isNil)
DEFINE_PREDICATE(ErrorP, "error?", &TypePredicates::error)
DEFINE_PREDICATE(ConsP, "cons?", &TypePredicates::isCons)
DEFINE_PREDICATE(ListP, "list?", &TypePredicates::isList)
DEFINE_PREDICATE(VectorP, "vector?", &TypePredicates::isVector)
DEFINE_PREDICATE(BoolP, "bool?", &TypePredicates::isBoolean)
DEFINE_PREDICATE(CharP, "char?", &TypePredicates::isChar)
DEFINE_PREDICATE(StringP, "string?", &TypePredicates::isString)
DEFINE_PREDICATE(SymbolP, "symbol?", &TypePredicates::isSymbol)
DEFINE_PREDICATE(KeywordP, "keyword?", &TypePredicates::isKeyword)
DEFINE_PREDICATE(NumericP, "numeric?", &TypePredicates::isNumeric)
DEFINE_PREDICATE(ComplexP, "complex?", &TypePredicates::isComplex)
DEFINE_PREDICATE(RealP, "real?", &TypePredicates::isReal)
DEFINE_PREDICATE(RationalP, "rational?", &TypePredicates::isRational)
DEFINE_PREDICATE(IntegerP, "integer?", &TypePredicates::isInteger)
DEFINE_PREDICATE(ExactP, "exact?", &TypePredicates::isExact)
DEFINE_PREDICATE(ProcP, "proc?", &TypePredicates::isProcedure)
DEFINE_PREDICATE(ClassP, "class?", &TypePredicates::isClass)
DEFINE_PREDICATE(InstanceP, "instance?", &TypePredicates::isInstance)
DEFINE_PREDICATE(DictP, "dict?", &TypePredicates::isDict)

#endif

namespace
{
    void apply_predicate(const char * name,
                         ExprPredicate pred,
                         ScamValue args,
                         Continuation * cont)
    {
        SingletonParser * parser = getSingletonOfAnythingParser();
        if ( ! parser->accept(args) ) {
            failedArgParseMessage(name, "(form)", args, cont);
        }
        else {
            ScamValue arg = parser->get();
            bool answer = pred(arg);
            ScamValue rv = ExpressionFactory::makeBoolean(answer);
            cont->run(rv);
        }
    }
}
