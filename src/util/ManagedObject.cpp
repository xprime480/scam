
#include "util/ManagedObject.hpp"

using namespace scam;

ManagedObject::ManagedObject()
    : marked(false)
{
}

ManagedObject::~ManagedObject()
{
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

