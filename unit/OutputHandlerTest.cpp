
#include "include/OutputHandlerTest.hpp"

#include "expr/ExpressionFactory.hpp"

using namespace scam;

OutputHandlerTest::OutputHandlerTest()
{
    reset();
}

void OutputHandlerTest::handleResult(ExprHandle expr)
{
    result = expr;
    last   = expr;
}

void OutputHandlerTest::handleError(ExprHandle expr)
{
    error = expr;
    last  = expr;
}

void OutputHandlerTest::handleTrace(std::string const & msg)
{
}

ExprHandle OutputHandlerTest::getLast() const
{
    return last;
}

ExprHandle OutputHandlerTest::getLastExpr() const
{
    return result;
}

ExprHandle OutputHandlerTest::getLastError() const
{
    return error;
}

void OutputHandlerTest::reset()
{
    result = error = last = ExpressionFactory::makeNull();
}
