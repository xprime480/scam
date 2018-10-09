#if ! defined(EXPRESSIONFACTORY_H)
#define EXPRESSIONFACTORY_H 1

#include "expr/ScamExpr.hpp"

#include <memory>
#include <string>
#include <vector>

namespace scam
{
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
        static ExprHandle
        makeCons(ExprHandle const & car, ExprHandle const & cdr);

        static ExprHandle makeVector(ExprVec const & elts);

        template <typename T, typename... Args>
        static ExprHandle makeForm(Args... args)
        {
            return std::make_shared<T>(args...);
        }
    };
}
#endif
