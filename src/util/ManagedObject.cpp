
#include "util/ManagedObject.hpp"

using namespace scam;

ManagedObject::ManagedObject()
    : managed(true)
    , marked(false)
{
}

ManagedObject::~ManagedObject()
{
}

void ManagedObject::setManaged(bool value) const
{
    managed = value;
}

bool ManagedObject::isManaged() const
{
    return managed;
}

void ManagedObject::mark() const
{
    marked = true;
}

bool ManagedObject::isMarked() const
{
    return marked;
}

void ManagedObject::unmark() const
{
    marked = false;
}
