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


DEFINE_PREDICATE(NilP, "nil?", &isNil)
DEFINE_PREDICATE(ErrorP, "error?", &error)
DEFINE_PREDICATE(ConsP, "cons?", &isCons)
DEFINE_PREDICATE(ListP, "list?", &isList)
DEFINE_PREDICATE(VectorP, "vector?", &isVector)
DEFINE_PREDICATE(BoolP, "bool?", &isBoolean)
DEFINE_PREDICATE(CharP, "char?", &isChar)
DEFINE_PREDICATE(StringP, "string?", &isString)
DEFINE_PREDICATE(SymbolP, "symbol?", &isSymbol)
DEFINE_PREDICATE(KeywordP, "keyword?", &isKeyword)
DEFINE_PREDICATE(NumericP, "numeric?", &isNumeric)
DEFINE_PREDICATE(ComplexP, "complex?", &isComplex)
DEFINE_PREDICATE(RealP, "real?", &isReal)
DEFINE_PREDICATE(RationalP, "rational?", &isRational)
DEFINE_PREDICATE(IntegerP, "integer?", &isInteger)
DEFINE_PREDICATE(ExactP, "exact?", &isExact)
DEFINE_PREDICATE(ProcP, "proc?", &isProcedure)
DEFINE_PREDICATE(ClassP, "class?", &isClass)
DEFINE_PREDICATE(InstanceP, "instance?", &isInstance)
DEFINE_PREDICATE(DictP, "dict?", &isDict)

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
