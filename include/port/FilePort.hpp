#if ! defined(FILEPORT_HPP)
#define FILEPORT_HPP 1

#include "port/ScamPort.hpp"

#include <fstream>
#include <string>

namespace scam
{
    class FilePort : public ScamPort
    {
    public:
        FilePort(const char * filename, unsigned int rw);
        ~FilePort();

        bool eof() const override;

        char getChar() override;
        size_t get(char * buf, size_t max) override;
        void putChar(char c) override;
        size_t put(const char * buf, size_t length) override;

        void rollback() override;

        std::string describe() override;
        ScamValue getContents() const override;

    private:
        const std::string filename;
        std::fstream stream;
    };
}

#endif
