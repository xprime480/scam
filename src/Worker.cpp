#include "Worker.hpp"

#include "util/GlobalId.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Worker::Worker(char const * id)
    : name(GlobalId::makeName(id))
{
}

Worker::~Worker()
{
}

Worker * Worker::makeInstance(char const * id)
{
    return new Worker(id);
}

void Worker::run()
{
}

string Worker::id() const
{
    return name;
}
