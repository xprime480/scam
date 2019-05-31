#if ! defined(CHARSTREAM_HPP)
#define CHARSTREAM_HPP 1

#include "ScamFwd.hpp"

#include <string>

namespace scam
{
    struct PositionType
    {
        unsigned long sequence;
        unsigned long offset;
    };

    class CharStream
    {
    public:
        virtual ~CharStream() {}

        virtual char peek() const = 0;
        virtual std::string strPeek(size_t n) const = 0;
        virtual char getCurrent() = 0;
        virtual void advance(size_t n = 1) = 0;
        virtual PositionType getPos() const = 0;
        virtual void setPos(PositionType newPos) = 0;
        virtual std::string allInput(PositionType where) const = 0;

        virtual std::string strBetween(PositionType from) const = 0;
        virtual std::string strBetween(PositionType from,
                                       PositionType to) const = 0;
    };

    extern bool operator==(const PositionType & lhs, const PositionType & rhs);
    extern bool operator!=(const PositionType & lhs, const PositionType & rhs);
    extern bool operator<(const PositionType & lhs, const PositionType & rhs);
    extern bool operator>(const PositionType & lhs, const PositionType & rhs);
    extern bool operator<=(const PositionType & lhs, const PositionType & rhs);
    extern bool operator>=(const PositionType & lhs, const PositionType & rhs);
}

#endif
