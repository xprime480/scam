#include "port/CinPort.hpp"

#include "ScamException.hpp"
#include "value/ValueFactory.hpp"

#include <iostream>

using namespace scam;
using namespace std;

CinPort::CinPort()
    : ScamPort(ScamPort::Readable)
{
}

CinPort::~CinPort()
{
}

bool CinPort::eof() const
{
    return cin.eof();
}

char CinPort::getChar()
{
    char ch;
    cin.get(ch);
    if ( eof() ) {
        return 0;
    }
    return ch;
}

size_t CinPort::get(char * buf, size_t max)
{
    cin.read(buf, max);
    if ( eof() ) {
        max = cin.gcount();
    }

    return max;
}

void CinPort::putChar(char c)
{
    throw ScamException("port is not writeable");
}

size_t CinPort::put(const char * buf, size_t length)
{
    throw ScamException("port is not writeable");
    return 0;
}

string CinPort::describe()
{
    stringstream s;
    s << "cin-port";
    return s.str();
}

ScamValue CinPort::getContents() const
{
    return makeString("?");
}
