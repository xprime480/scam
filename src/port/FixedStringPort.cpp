#include "port/FixedStringPort.hpp"

#include "ScamException.hpp"
#include "expr/ValueFactory.hpp"

#include <cstdlib>
#include <cstring>

using namespace scam;
using namespace std;

FixedStringPort::FixedStringPort(const char * initial)
    : ScamPort(ScamPort::Readable)
    , nextRead(0)
    , contents(initial)
{
}

FixedStringPort::~FixedStringPort()
{
}

bool FixedStringPort::eof() const
{
    return nextRead == contents.size();
}

char FixedStringPort::getChar()
{
    if ( eof() ) {
        return 0;
    }
    else {
        return contents.at(nextRead++);
    }
}

size_t FixedStringPort::get(char * buf, size_t max)
{
    size_t current = contents.size() - nextRead;
    if ( max > current ) {
        max = current;
    }

    if ( 0 == max || ! buf ) {
        return 0;
    }

    memcpy(buf, contents.c_str() + nextRead, max);
    nextRead += max;
    return max;
}

void FixedStringPort::putChar(char c)
{
    throw ScamException("port is not writeable");
}

size_t FixedStringPort::put(const char * buf, size_t length)
{
    throw ScamException("port is not writeable");
}

string FixedStringPort::describe()
{
    string tmp;
    if ( nextRead >= contents.size() ) {
        tmp = "";
    }
    else {
        tmp = contents.substr(nextRead);
    }

    stringstream s;
    s << "fixed-string-port<" << tmp << ">";
    return s.str();
}

ScamValue FixedStringPort::getContents() const
{
    string tmp;
    if ( nextRead >= contents.size() ) {
        tmp = "";
    }
    else {
        tmp = contents.substr(nextRead);
    }

    return makeString(tmp);
}
