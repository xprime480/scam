
#include "util/ManagedObject.hpp"

using namespace scam;

ManagedObject::ManagedObject()
    : marked(false)
{
}

ManagedObject::~ManagedObject()
{
}

bool ManagedObject::isManaged() const
{
    return true;
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
