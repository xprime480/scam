#if ! defined(OUTPUTHANDLER_HPP)
#define OUTPUTHANDLER_HPP 1

#include <string>

namespace scam
{
    class OutputHandler
    {
    public:
        virtual ~OutputHandler() {}

        virtual void handleResult(std::string const & result) = 0;
        virtual void handleError(std::string const & error) = 0;
        virtual void handleTrace(std::string const & msg) = 0;
    };
}

#endif
