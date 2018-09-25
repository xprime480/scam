#if ! defined(OUTPUTHANDLERBUFFERED_HPP)
#define OUTPUTHANDLERBUFFERED_HPP 1

#include "output/OutputHandler.hpp"

#include <sstream>

namespace scam
{
    class OutputHandlerBuffered : public OutputHandler
    {
    public:
        OutputHandlerBuffered();

        void handleResult(std::string const & result) override;
        void handleError(std::string const & error) override;
        void handleTrace(std::string const & msg) override;

        std::string get() const;
        void reset();

    private:
        std::stringstream buffer;
    };
}

#endif


