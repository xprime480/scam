#if ! defined(SCAMPORT_HPP)
#define SCAMPORT_HPP 1

#include "ScamFwd.hpp"

#include <cstddef>
#include <iostream>
#include <string>

namespace scam
{
    class ScamPort
    {
    public:
        static constexpr unsigned int Readable = 1;
        static constexpr unsigned int Writeable = 2;

        ScamPort(unsigned int rw);
        virtual ~ScamPort();

        bool isReadable() const;
        bool isWriteable() const;

        virtual bool eof() const = 0;

        virtual char getChar() = 0;
        virtual size_t get(char * buf, size_t max) = 0;
        virtual void putChar(char c) = 0;
        virtual size_t put(const char * buf, size_t length) = 0;

        virtual void rollback();
        virtual void advance(unsigned int count);

        virtual std::string describe() = 0;
        virtual ScamValue getContents() const = 0;

    protected:
        unsigned int checkpoint;

    private:
        unsigned int rw;
    };
}

#endif
