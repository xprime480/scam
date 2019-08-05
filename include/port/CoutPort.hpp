#if ! defined(COUTPORT_HPP)
#define COUTPORT_HPP 1

#include "port/ScamPort.hpp"

namespace scam
{
    class CoutPort : public ScamPort
    {
    public:
        CoutPort();
        ~CoutPort();

        bool eof() const override;

        char getChar() override;
        size_t get(char * buf, size_t max) override;
        void putChar(char c) override;
        size_t put(const char * buf, size_t length) override;

        std::string describe() override;
        ScamValue getContents() const override;
    };
}

#endif
