#include "port/CoutPort.hpp"

#include "ScamException.hpp"
#include "expr/ValueFactory.hpp"

#include <iostream>

using namespace scam;
using namespace std;

CoutPort::CoutPort()
    : ScamPort(ScamPort::Writeable)
{
}

CoutPort::~CoutPort()
{
}

bool CoutPort::eof() const
{
    return cout.eof();
}

char CoutPort::getChar()
{
    throw ScamException("port is not readable");
    return 0;
}

size_t CoutPort::get(char * buf, size_t max)
{
    throw ScamException("port is not readable");
    return 0;
}

void CoutPort::putChar(char c)
{
    cout.put(c);
}

size_t CoutPort::put(const char * buf, size_t length)
{
    if ( 0 == length ) {
        return 0;
    }
    if ( ! cout.good() ) {
        return 0;
    }

    cout.write(buf, length);
    return length;
}

string CoutPort::describe()
{
    return "cout-port";
}

ScamValue CoutPort::getContents() const
{
    return makeString("?");
}
