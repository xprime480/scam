#include "port/CerrPort.hpp"

#include "ScamException.hpp"
#include "expr/ValueFactory.hpp"

#include <iostream>

using namespace scam;
using namespace std;

CerrPort::CerrPort()
    : ScamPort(ScamPort::Writeable)
{
}

CerrPort::~CerrPort()
{
}

bool CerrPort::eof() const
{
    return cerr.eof();
}

char CerrPort::getChar()
{
    throw ScamException("port is not readable");
    return 0;
}

size_t CerrPort::get(char * buf, size_t max)
{
    throw ScamException("port is not readable");
    return 0;
}

void CerrPort::putChar(char c)
{
    cerr.put(c);
}

size_t CerrPort::put(const char * buf, size_t length)
{
    if ( 0 == length ) {
        return 0;
    }
    if ( ! cerr.good() ) {
        return 0;
    }

    cerr.write(buf, length);
    return length;
}

string CerrPort::describe()
{
    return "cerr-port";
}

ScamValue CerrPort::getContents() const
{
    return makeString("?");
}
