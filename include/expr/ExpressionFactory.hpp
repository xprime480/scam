#if ! defined(EXPRESSIONFACTORY_H)
#define EXPRESSIONFACTORY_H 1

#include "expr/ScamExpr.hpp"

#include <memory>
#include <string>
#include <vector>

namespace scam
{
    class ExpressionFactory
    {
    public:
        static std::shared_ptr<ScamExpr> makeNull();

        static std::shared_ptr<ScamExpr> makeError(char const * msg);
        static std::shared_ptr<ScamExpr> makeError(std::string const & msg);

        static std::shared_ptr<ScamExpr> makeBoolean(bool value);
        static std::shared_ptr<ScamExpr>
        makeCharacter(std::string const & value);

        static std::shared_ptr<ScamExpr> makeString(std::string const & value);
        static std::shared_ptr<ScamExpr> makeSymbol(std::string const & value);

        static std::shared_ptr<ScamExpr> makeFloat(double value);
        static std::shared_ptr<ScamExpr> makeInteger(int value);

        static std::shared_ptr<ScamExpr> makeNil();
        static std::shared_ptr<ScamExpr>
        makeCons(std::shared_ptr<ScamExpr> const & car,
                 std::shared_ptr<ScamExpr> const & cdr);

        static std::shared_ptr<ScamExpr>
        makeVector(std::vector<std::shared_ptr<ScamExpr>> const & elts);

        template <typename T, typename... Args>
        static std::shared_ptr<ScamExpr> makeForm(Args... args)
        {
            return std::make_shared<T>(args...);
        }
    };
}

#endif
