#include "input/StringCharStream.hpp"

using namespace scam;
using namespace std;

StringCharStream::StringCharStream(const string & input)
    : input(input)
    , base(this->input.c_str())
    , offset(0)
    , end(input.size())
{
}

void StringCharStream::mark()
{
}

char StringCharStream::peek() const
{
    if ( offset < end ) {
        return base[offset];
    }
    return 0;
}

string StringCharStream::strPeek(size_t n) const
{
    size_t i = min(n, end - offset);
    return string(base + offset, i);
}


char StringCharStream::getCurrent()
{
    char c = peek();
    if ( c ) {
        ++offset;
    }
    return c;
}

void StringCharStream::advance(size_t n)
{
    size_t i = min(n, end - offset);
    offset += i;
}

PositionType StringCharStream::getPos() const
{
    return PositionType { 0, offset };
}

void StringCharStream::setPos(PositionType newPos)
{
    offset = newPos.offset;
}

string StringCharStream::allInput(PositionType where) const
{
    return string(base + where.offset, end - where.offset);
}

string StringCharStream::strBetween(PositionType from) const
{
    return strBetween(from, getPos());
}

string StringCharStream:: strBetween(PositionType from, PositionType to) const
{
    if ( from >= to ) {
        return "";
    }

    auto len = to.offset - from.offset;
    string rv(base + from.offset, len);
    return rv;
}
