#if ! defined(FIXEDSTRINGPORT_HPP)
#define FIXEDSTRINGPORT_HPP 1

#include "port/ScamPort.hpp"

#include <string>

namespace scam
{
    class FixedStringPort : public ScamPort
    {
    public:
        FixedStringPort(const char * initial);
        ~FixedStringPort();

        bool eof() const override;

        char getChar() override;
        size_t get(char * buf, size_t max) override;
        void putChar(char c) override;
        size_t put(const char * buf, size_t length) override;

        std::string describe() override;
        ScamValue getContents() const override;

    private:
        size_t nextRead;
        std::string contents;
    };
}

#endif
