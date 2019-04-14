#include "Worker.hpp"

#include "util/DebugTrace.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Worker::Worker(char const * id)
    : name(makeName(id))
{
    scamTrace("Creating worker ", this, name);
}

Worker::~Worker()
{
    scamTrace("Deleting worker ", name);
}

Worker * Worker::makeInstance(char const * id)
{
    return new Worker(id);
}

void Worker::run()
{
    scamTrace("Executing worker ", name);
}

string Worker::id() const
{
    return name;
}

string Worker::makeName(char const * id)
{
    static unsigned counter { 0 };
    stringstream s;
    s << (++counter) << " {" << id << "}";
    return s.str();
}
