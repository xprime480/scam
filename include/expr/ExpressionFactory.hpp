#if ! defined(EXPRESSIONFACTORY_H)
#define EXPRESSIONFACTORY_H 1

#include "expr/ScamExpr.hpp"

#include <memory>
#include <string>

namespace scam
{
    class ExpressionFactory
    {
    public:
        static std::shared_ptr<ScamExpr> makeNull();

        static std::shared_ptr<ScamExpr> makeError(char const * msg);
        static std::shared_ptr<ScamExpr> makeError(std::string const & msg);

        static std::shared_ptr<ScamExpr> makeBoolean(bool value);
        static std::shared_ptr<ScamExpr> makeFloat(double value);
        static std::shared_ptr<ScamExpr> makeInteger(int value);
    };
}

#endif
