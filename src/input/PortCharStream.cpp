#include "input/PortCharStream.hpp"

#include "ScamException.hpp"
#include "port/ScamPort.hpp"
#include "value/ScamData.hpp"
#include "value/ScamToInternal.hpp"

#include <cstdlib>

using namespace scam;
using namespace std;

PortCharStream::PortCharStream(ScamValue value)
    : value(value)
    , port(asPort(value))
    , buffer(nullptr)
    , offset(0)
    , capacity(0)
{
    buffer = (char *) malloc(256);
    capacity = port->get(buffer, 255);
    buffer[capacity] = 0;
}

PortCharStream::~PortCharStream()
{
    free(buffer);
}

void PortCharStream::mark()
{
    value->mark();
}

char PortCharStream::peek() const
{
    fillBuffer(1);
    return buffer[offset];
}

string PortCharStream::strPeek(size_t n) const
{
    fillBuffer(n);
    const char * pos = buffer + offset;
    const char * tmp = pos;

    size_t i = 0;
    while ( *tmp++ && i < n ) {
        ++i;
    }

    return string(pos, i);
}

char PortCharStream::getCurrent()
{
    fillBuffer(1);
    return buffer[offset++];
}

void PortCharStream::advance(size_t n)
{
    port->advance(n);
    offset += fillBuffer(n);
}

PositionType PortCharStream::getPos() const
{
    return PositionType { 0u, offset };
}

void PortCharStream::setPos(PositionType newPos)
{
    // for now do not care about sequence
    offset = newPos.offset;
}

string PortCharStream::allTextStartingAt(PositionType where) const
{
    fillBuffer(0);
    // for now do not care about sequence
    return string(buffer + where.offset);
}

string PortCharStream::strBetween(PositionType from) const
{
    // for now do not care about sequence
    return strBetween(from, getPos());
}

string PortCharStream:: strBetween(PositionType from, PositionType to) const
{
    // for now do not care about sequence
    if ( from.offset >= to.offset ) {
        return "";
    }

    auto len = to.offset - from.offset;
    string rv(buffer + from.offset, len);
    return rv;
}

size_t PortCharStream::fillBuffer(size_t n) const
{
    if ( 0 == n ) {
        // we want all the input from the port.  If the port is
        // infinite, we will never return until we run out of memory;
        // if the port is blocked, we hang
        while ( ! port->eof() ) {
            size_t old = capacity;
            fillBuffer(10240);
            if ( old == capacity ) {
                break;          // hack to avoid the above problems
            }
        }
        return capacity - offset;
    }
    else {
        size_t newCapacity = offset + n;
        if ( newCapacity <= capacity ) {
            return n;           // we already have enough buffered
        }
        if ( port->eof() ) {
            return capacity - offset; // give as much as we already have
        }

        size_t need = newCapacity - capacity;
        if ( 0 != (need & 0xff) ) {
            need = (1 + ((need & 0xff) >> 8)) << 8;
        }

        char * tmp = (char *) realloc(buffer, capacity + need + 1);
        if ( ! tmp ) {
            throw ScamException("stream has no internal capacity");
        }

        buffer = tmp;
        size_t received = port->get(buffer + capacity, need);
        capacity += received;
        buffer[capacity] = 0;
        return min(n, capacity - offset);
    }
}
