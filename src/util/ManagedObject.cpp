
#include "util/ManagedObject.hpp"

using namespace scam;

ManagedObject::ManagedObject(bool managed)
    : managed(managed)
    , marked(false)
{
}

ManagedObject::~ManagedObject()
{
}

bool ManagedObject::isManaged() const
{
    return managed;
}

void ManagedObject::mark()
{
    marked = true;
}

bool ManagedObject::isMarked() const
{
    return marked;
}

void ManagedObject::unmark()
{
    marked = false;
}
