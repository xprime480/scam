#include "port/StringPort.hpp"

#include "ScamException.hpp"
#include "expr/ValueFactory.hpp"

#include <algorithm>
#include <cstdlib>
#include <cstring>

using namespace scam;
using namespace std;

StringPort::StringPort(const char * initial, unsigned int rw)
    : ScamPort(rw)
    , nextRead(0)
    , nextWrite(initial ? strlen(initial) : 0u)
    , capacity(max(2u * nextWrite, size_t(256u)))
    , buffer((char *) malloc(capacity))
{
    if ( ! buffer ) {
        throw ScamException("port cannot get memory");
    }

    if ( initial ) {
        strcpy(buffer, initial);
    }
}

StringPort::~StringPort()
{
    free(buffer);
}

bool StringPort::eof() const
{
    if ( ! isReadable() ) {
        return true;
    }
    if ( ! isWriteable() ) {
        return nextWrite == nextRead;
    }
    return false;
}

char StringPort::getChar()
{
    if ( ! isReadable() ) {
        throw ScamException("port is not readable");
    }

    if ( nextRead < nextWrite ) {
        return buffer[nextRead++];
    }
    else {
        return 0;
    }
}

size_t StringPort::get(char * buf, size_t max)
{
    if ( ! isReadable() ) {
        throw ScamException("port is not readable");
    }

    size_t current = nextWrite - nextRead;
    if ( max > current ) {
        max = current;
    }

    if ( 0 == max || ! buf ) {
        return 0;
    }

    memcpy(buf, buffer + nextRead, max);
    nextRead += max;
    return max;
}

void StringPort::putChar(char c)
{
    if ( ! isWriteable() ) {
        throw ScamException("port is not writeable");
    }

    expandBuffer(1);
    buffer[nextWrite++] = c;
}

size_t StringPort::put(const char * buf, size_t length)
{
    if ( ! isWriteable() ) {
        throw ScamException("port is not writeable");
    }

    if ( 0 == length ) {
        return 0;
    }

    expandBuffer(length);
    memcpy(buffer + nextWrite, buf, length);
    nextWrite += length;
    return length;
}

string StringPort::describe()
{
    stringstream s;
    s << "string-port<"
      << string(buffer + nextRead, nextWrite - nextRead)
      << ">";
    return s.str();
}

ScamValue StringPort::getContents() const
{
    string contents(buffer + nextRead, nextWrite - nextRead);
    return makeString(contents);
}

void StringPort::expandBuffer(size_t needed)
{
    if ( (capacity - nextWrite) > needed ) {
        return;
    }

    while ( (capacity - nextWrite) <= needed ) {
        capacity *= 2;
    }

    char * tmp = (char *) realloc(buffer, capacity);
    if ( ! tmp ) {
        throw ScamException("port has no capacity");
    }
}
