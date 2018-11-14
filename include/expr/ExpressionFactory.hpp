#if ! defined(EXPRESSIONFACTORY_H)
#define EXPRESSIONFACTORY_H 1

#include "expr/ScamExpr.hpp"

#include <memory>
#include <string>
#include <vector>

namespace scam
{
    class Continuation;
    using ContHandle = std::shared_ptr<Continuation>;

    using ExprVec = std::vector<ExprHandle>;

    class ExpressionFactory
    {
    public:
        static ExprHandle makeNull();

        static ExprHandle makeError(char const * msg);
        static ExprHandle makeError(std::string const & msg);

        static ExprHandle makeBoolean(bool value);
        static ExprHandle makeCharacter(std::string const & value);

        static ExprHandle makeString(std::string const & value);
        static ExprHandle makeSymbol(std::string const & value);

        static ExprHandle makeFloat(double value);
        static ExprHandle makeInteger(int value);

        static ExprHandle makeNil();
        static ExprHandle makeCons(ScamExpr * car, ScamExpr * cdr);

        static ExprHandle makeList();
        static ExprHandle makeList(ScamExpr * item);

        template <typename... Args>
        static ExprHandle makeList(ScamExpr * car, Args... args)
        {
            return makeCons(car, makeList(args...).get());
        }

        static ExprHandle makeVector(ExprVec const & elts);

        static ExprHandle makeClosure(ScamExpr * formals,
                                      ScamExpr * forms,
                                      Env env,
                                      bool macrolike = false);

        static ExprHandle makeClass(ScamExpr * base,
                                    ScamExpr * vars,
                                    ScamExpr * funs,
                                    Env env);

        static ExprHandle makeInstance(ScamExpr * vars,
                                       ScamExpr * funs,
                                       Env env);

        static ExprHandle makeContinuation(ContHandle cont);

        template <typename T, typename... Args>
        static ExprHandle makeForm(Args... args)
        {
            return intern(std::make_shared<T>(args...));
        }

        template <typename T>
        static ExprHandle makeForm()
        {
            static const ExprHandle cached = intern(std::make_shared<T>());
            return cached;
        }

        static ExprHandle clone(ScamExpr const *);

        static unsigned getMaxHandles();

    private:
        static ExprHandle intern(ExprHandle expr);
    };
}
#endif
