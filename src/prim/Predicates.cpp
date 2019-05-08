#include "prim/Predicates.hpp"

#include "Continuation.hpp"
#include "ScamFwd.hpp"
#include "expr/ExpressionFactory.hpp"
#include "input/SingletonParser.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

namespace
{
    using ExprPredicate = bool (ScamExpr::*)() const;

    extern void apply_predicate(const char * name,
                                ExprPredicate pred,
                                ExprHandle args,
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
    void cls::applyArgs(ExprHandle args, Continuation * cont)   \
    {                                                           \
        apply_predicate(label, pred, args, cont);               \
    }                                                           \
    bool cls::equals(ConstExprHandle expr) const                \
    {                                                           \
        return ( expr && expr->toString() == label );           \
    }


DEFINE_PREDICATE(NilP, "nil?", &ScamExpr::isNil)
DEFINE_PREDICATE(ErrorP, "error?", &ScamExpr::error)
DEFINE_PREDICATE(ConsP, "cons?", &ScamExpr::isCons)
DEFINE_PREDICATE(ListP, "list?", &ScamExpr::isList)
DEFINE_PREDICATE(VectorP, "vector?", &ScamExpr::isVector)
DEFINE_PREDICATE(BoolP, "bool?", &ScamExpr::isBoolean)
DEFINE_PREDICATE(CharP, "char?", &ScamExpr::isChar)
DEFINE_PREDICATE(StringP, "string?", &ScamExpr::isString)
DEFINE_PREDICATE(SymbolP, "symbol?", &ScamExpr::isSymbol)
DEFINE_PREDICATE(KeywordP, "keyword?", &ScamExpr::isKeyword)
DEFINE_PREDICATE(NumericP, "numeric?", &ScamExpr::isNumeric)
DEFINE_PREDICATE(ComplexP, "complex?", &ScamExpr::isComplex)
DEFINE_PREDICATE(RealP, "real?", &ScamExpr::isReal)
DEFINE_PREDICATE(RationalP, "rational?", &ScamExpr::isRational)
DEFINE_PREDICATE(IntegerP, "integer?", &ScamExpr::isInteger)
DEFINE_PREDICATE(ProcP, "proc?", &ScamExpr::isProcedure)
DEFINE_PREDICATE(ClassP, "class?", &ScamExpr::isClass)
DEFINE_PREDICATE(InstanceP, "instance?", &ScamExpr::isInstance)
DEFINE_PREDICATE(DictP, "dict?", &ScamExpr::isDict)

#endif

namespace
{
    void apply_predicate(const char * name,
                         ExprPredicate pred,
                         ExprHandle args,
                         Continuation * cont)
    {
        SingletonParser * parser = getSingletonOfAnythingParser();
        if ( ! parser->accept(args) ) {
            failedArgParseMessage(name, "(form)", args, cont);
        }
        else {
            ExprHandle arg = parser->get();
            bool answer = (arg->*pred)();
            ExprHandle rv = ExpressionFactory::makeBoolean(answer);
            cont->run(rv);
        }
    }
}
