#if ! defined(OUTPUTHANDLER_HPP)
#define OUTPUTHANDLER_HPP 1

#include <memory>
#include <string>

namespace scam
{
    class ScamExpr;
    using ExprHandle = std::shared_ptr<ScamExpr>;

    class OutputHandler
    {
    public:
        virtual ~OutputHandler() {}

        virtual void handleResult(ExprHandle expr) = 0;
        virtual void handleError(ExprHandle expr) = 0;
        virtual void handleTrace(std::string const & msg) = 0;
    };
}

#endif
