#include "Worker.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Worker::Worker(char const * id, ScamEngine * engine)
    : name(makeName(id))
    , engine(engine)
{
}

Worker::~Worker()
{
}

Worker * Worker::makeInstance(char const * id, ScamEngine * engine)
{
    return new Worker(id, engine);
}

void Worker::run()
{
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
