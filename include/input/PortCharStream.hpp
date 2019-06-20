#if ! defined(PORTCHARSTREAM_HPP)
#define PORTCHARSTREAM_HPP 1

#include "input/CharStream.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ScamPort;

    class PortCharStream : public CharStream
    {
    public:
        PortCharStream(ScamValue value);
        ~PortCharStream();

        void mark() override;

        char peek() const override;
        std::string strPeek(size_t n) const override;
        char getCurrent() override;
        void advance(size_t n = 1) override;
        PositionType getPos() const override;
        void setPos(PositionType newPos) override;
        std::string allTextStartingAt(PositionType where) const override;

        std::string strBetween(PositionType from) const override;
        std::string strBetween(PositionType from,
                               PositionType to) const override;

    private:
        ScamValue value;
        ScamPort * port;
        mutable char * buffer;
        size_t offset;
        mutable size_t capacity;

        /*
         * n is how many bytes we want to be filled beyond the current
         * offset.  Return value indicates how many we get.
         */
        size_t fillBuffer(size_t n) const;

    };
}

#endif
