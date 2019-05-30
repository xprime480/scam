#if ! defined(SCAMPORT_HPP)
#define SCAMPORT_HPP 1

namespace scam
{
    class ScamPort
    {
    public:
        unsigned int Readable = 1;
        unsigned int Writeable = 2;

        ScamPort(unsigned int rw);
        virtual ~ScamPort() {}

        virtual char getChar() = 0;
        virtual size_t get(char * buf, size_t max) = 0;
        virtual void putChar(char c) = 0;
        virtual size_t put(char * buf, size_t length) = 0;

        bool isReadable() const final;
        bool isWriteable() const final;

    private:
        const unsigned int rw;
    };
}

#endif
