#if ! defined(EXPRESSIONFACTORY_H)
#define EXPRESSIONFACTORY_H 1

#include <memory>
#include <string>
#include <vector>

#include "expr/ScamExprAll.hpp"

#include "util/MemoryManager.hpp"

namespace scam
{
    class Continuation;
    using ContHandle = std::shared_ptr<Continuation>;

    using ExprVec = std::vector<ScamExpr *>;

    class ExpressionFactory
    {
    public:
        static ScamNull * makeNull();

        static ScamError * makeError(char const * msg);
        static ScamError * makeError(std::string const & msg);

        static ScamBoolean * makeBoolean(bool value);
        static ScamCharacter * makeCharacter(std::string const & value);

        static ScamString * makeString(std::string const & value);
        static ScamSymbol * makeSymbol(std::string const & value);
        static ScamKeyword * makeKeyword(std::string const & value);

        static ScamFloat * makeFloat(double value);
        static ScamInteger * makeInteger(int value);

        static ScamNil * makeNil();
        static ScamCons * makeCons(ScamExpr * car, ScamExpr * cdr);

        static ScamExpr * makeList();
        static ScamExpr * makeList(ScamExpr * item);

        template <typename... Args>
        static ScamExpr * makeList(ScamExpr * car, Args... args)
        {
            return makeCons(car, makeList(args...));
        }

        static ScamVector * makeVector(ExprVec const & elts);

        static ScamClosure * makeClosure(ScamExpr * formals,
                                         ScamExpr * forms,
                                         Env env,
                                         bool macrolike = false);

        static ScamClass * makeClass(ScamExpr * base,
                                     ScamExpr * vars,
                                     ScamExpr * funs,
                                     Env env);

        static ScamInstance * makeInstance(ScamExpr * vars,
                                           ScamExpr * funs,
                                           Env env);

        static ScamContinuation * makeContinuation(ContHandle cont);

        static ScamDict * makeDict();
        static ScamDict * makeDict(ExprVec const & args);

        template <typename T, typename... Args>
        static T * makeForm(Args... args)
        {
            return mm.make<T>(args...);
        }

    private:
        static MemoryManager mm;
    };
}
#endif
