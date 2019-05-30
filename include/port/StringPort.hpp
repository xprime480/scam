#if ! defined(STRINGPORT_HPP)
#define STRINGPORT_HPP 1

#include "port/ScamPort.hpp"

namespace scam
{
    class StringPort : public ScamPort
    {
    public:
        StringPort(const char * initial, unsigned int rw);
        ~StringPort();

        bool eof() const override;

        char getChar() override;
        size_t get(char * buf, size_t max) override;
        void putChar(char c) override;
        size_t put(const char * buf, size_t length) override;

        std::string describe() override;
        ScamValue getContents() const override;

    private:
        size_t nextRead;
        size_t nextWrite;
        size_t capacity;
        char * buffer;

        void expandBuffer(size_t needed);
    };
}

#endif
