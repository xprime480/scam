
#include "output/OutputHandlerBuffered.hpp"

#include "expr/ScamExpr.hpp"

using namespace scam;

OutputHandlerBuffered::OutputHandlerBuffered()
{
}

void OutputHandlerBuffered::handleResult(ExprHandle expr)
{
    buffer << expr->toString() << "\n";
}

void OutputHandlerBuffered::handleError(ExprHandle expr)
{
    buffer << expr->toString() << "\n";
}

void OutputHandlerBuffered::handleTrace(std::string const & msg)
{
}

std::string OutputHandlerBuffered::get() const
{
    return buffer.str();
}

void OutputHandlerBuffered::reset()
{
    buffer.str("");
}
