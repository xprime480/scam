#include "port/ScamPort.hpp"

using namespace scam;

ScamPort::ScamPort(unsigned int rw)
    : rw(rw)
{
}

ScamPort::~ScamPort()
{
}

bool ScamPort::isReadable() const
{
    return 0 != (rw & Readable);
}

bool ScamPort::isWriteable() const
{
    return 0 != (rw & Writeable);
}
