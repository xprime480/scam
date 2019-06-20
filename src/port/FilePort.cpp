#include "port/FilePort.hpp"

#include "ScamException.hpp"
#include "expr/ValueFactory.hpp"

#include <sstream>
#include <unistd.h>

using namespace scam;
using namespace std;

FilePort::FilePort(const char * filename, unsigned int rw)
    : ScamPort(rw)
    , filename(filename)
{
    if ( isReadable() && isWriteable() ) {
        stream.open(filename);
    }
    if ( isReadable() ) {
        stream.open(filename, ios_base::in);
    }
    else if ( isWriteable() ) {
        stream.open(filename, ios_base::out);
    }

    if ( ! stream.good() ) {
        stringstream s;
        char buf[1024];
        s << "Unable to open file " << filename
          << " (cwd = " << getcwd(buf, 1024) << ")";
        throw ScamException(s.str());
    }
}

FilePort::~FilePort()
{
    stream.close();
}

bool FilePort::eof() const
{
    return stream.eof();
}

char FilePort::getChar()
{
    if ( ! isReadable() ) {
        throw ScamException("port is not readable");
    }

    char ch;
    stream.get(ch);
    if ( eof() ) {
        return 0;
    }
    return ch;
}

size_t FilePort::get(char * buf, size_t max)
{
    if ( ! isReadable() ) {
        throw ScamException("port is not readable");
    }

    stream.read(buf, max);
    if ( eof() ) {
        max = stream.gcount();
    }

    return max;
}

void FilePort::putChar(char c)
{
    if ( ! isWriteable() ) {
        throw ScamException("port is not writeable");
    }

    stream.put(c);
}

size_t FilePort::put(const char * buf, size_t length)
{
    if ( ! isWriteable() ) {
        throw ScamException("port is not writeable");
    }

    if ( 0 == length ) {
        return 0;
    }
    if ( ! stream.good() ) {
        return 0;
    }

    stream.write(buf, length);
    return length;
}

void FilePort::rollback()
{
    throw ScamException("FilePort::rollback: not implemented");
}

string FilePort::describe()
{
    stringstream s;
    s << "file-port<" << filename << ">";
    return s.str();
}

ScamValue FilePort::getContents() const
{
    throw ScamException("FilePort::getContents: not implemented");
    return makeNothing();
}
