#if ! defined(STRINGCHARSTREAM_HPP)
#define STRINGCHARSTREAM_HPP 1

#include "input/CharStream.hpp"

namespace scam
{
    class StringCharStream : public CharStream
    {
    public:
        StringCharStream(std::string const & input);

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
        const std::string input;
        const char * base;
        unsigned long int offset;
        unsigned long int end;
    };
}

#endif
