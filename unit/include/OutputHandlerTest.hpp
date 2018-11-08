#if ! defined(OUTPUTHANDLERTEST_HPP)
#define OUTPUTHANDLERTEST_HPP 1

#include "output/OutputHandler.hpp"

#include <sstream>

namespace scam
{
    class OutputHandlerTest : public OutputHandler
    {
    public:
        OutputHandlerTest();

        void handleResult(ExprHandle expr) override;
        void handleError(ExprHandle expr) override;
        void handleTrace(std::string const & msg) override;

        ExprHandle getLast() const;
        ExprHandle getLastExpr() const;
        ExprHandle getLastError() const;

        void reset();

    private:
	ExprHandle result;
	ExprHandle error;
	ExprHandle last;
    };
}

#endif


