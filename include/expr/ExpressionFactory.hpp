#if ! defined(EXPRESSIONFACTORY_H)
#define EXPRESSIONFACTORY_H 1

#include "expr/ScamExpr.hpp"

#include "util/MemoryManager.hpp"

#include <memory>
#include <string>
#include <vector>

namespace scam
{
    class Continuation;
    using ContHandle = std::shared_ptr<Continuation>;

    using ExprVec = std::vector<ScamExpr *>;

    class ExpressionFactory
    {
    public:
        static ScamExpr * makeNull();

        static ScamExpr * makeError(char const * msg);
        static ScamExpr * makeError(std::string const & msg);

        static ScamExpr * makeBoolean(bool value);
        static ScamExpr * makeCharacter(std::string const & value);

        static ScamExpr * makeString(std::string const & value);
        static ScamExpr * makeSymbol(std::string const & value);
        static ScamExpr * makeKeyword(std::string const & value);

        static ScamExpr * makeFloat(double value);
        static ScamExpr * makeInteger(int value);

        static ScamExpr * makeNil();
        static ScamExpr * makeCons(ScamExpr * car, ScamExpr * cdr);

        static ScamExpr * makeList();
        static ScamExpr * makeList(ScamExpr * item);

        template <typename... Args>
        static ScamExpr * makeList(ScamExpr * car, Args... args)
        {
            return makeCons(car, makeList(args...));
        }

        static ScamExpr * makeVector(ExprVec const & elts);

        static ScamExpr * makeClosure(ScamExpr * formals,
                                      ScamExpr * forms,
                                      Env env,
                                      bool macrolike = false);

        static ScamExpr * makeClass(ScamExpr * base,
                                    ScamExpr * vars,
                                    ScamExpr * funs,
                                    Env env);

        static ScamExpr * makeInstance(ScamExpr * vars,
                                       ScamExpr * funs,
                                       Env env);

        static ScamExpr * makeContinuation(ContHandle cont);

        static ScamExpr * makeDict();
        static ScamExpr * makeDict(ExprVec const & args);

        template <typename T, typename... Args>
        static ScamExpr * makeForm(Args... args)
        {
            return mm.make<T>(args...);
        }

    private:
        static MemoryManager mm;
    };
}
#endif
