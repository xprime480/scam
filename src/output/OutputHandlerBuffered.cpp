
#include "output/OutputHandlerBuffered.hpp"

using namespace scam;

OutputHandlerBuffered::OutputHandlerBuffered()
{
}

void OutputHandlerBuffered::handleResult(std::string const & result)
{
    buffer << result << "\n";
}

void OutputHandlerBuffered::handleError(std::string const & error)
{
    buffer << error << "\n";
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

