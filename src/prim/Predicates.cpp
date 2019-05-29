#include "prim/Predicates.hpp"

#include "Continuation.hpp"
#include "ScamFwd.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "input/SingletonParser.hpp"
#include "util/ArgListHelper.hpp"

#include <functional>

using namespace scam;
using namespace std;

namespace
{
    using ExprPredicate = function<bool(const ScamData *)>;
}


#if defined(DEFINE_PREDICATE)
#error "DEFINE_PREDICATE already defined"
#else

#define DEFINE_PREDICATE(cls, label, pred)                      \
        void scam::apply##cls(ScamValue args,                   \
                        Continuation * cont,                    \
                        ScamEngine * engine)                    \
        {                                                       \
            static const string name { label };                 \
            SingletonParser * parser = getSingletonOfAnythingParser(); \
            if ( ! parser->accept(args) ) {                     \
                failedArgParseMessage(name.c_str(), "(form)", args, cont); \
            }                                                   \
            else {                                              \
                ScamValue arg = parser->get();                  \
                bool answer = pred(arg);                        \
                ScamValue rv = makeBoolean(answer);             \
                cont->run(rv);                                  \
            }                                                   \
        }

DEFINE_PREDICATE(NilP, "nil?", isNil)
DEFINE_PREDICATE(ErrorP, "error?", error)
DEFINE_PREDICATE(ConsP, "cons?", isCons)
DEFINE_PREDICATE(ListP, "list?", isList)
DEFINE_PREDICATE(VectorP, "vector?", isVector)
DEFINE_PREDICATE(BoolP, "bool?", isBoolean)
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
