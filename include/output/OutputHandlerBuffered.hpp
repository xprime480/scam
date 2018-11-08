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

        void handleResult(ExprHandle) override;
        void handleError(ExprHandle) override;
        void handleTrace(std::string const & msg) override;

        std::string get() const;
        void reset();

    private:
        std::stringstream buffer;
    };
}

#endif


