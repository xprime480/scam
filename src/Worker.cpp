#include "Worker.hpp"

#include "util/GlobalId.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Worker::Worker(char const * id, ScamEngine * engine)
    : name(GlobalId::makeName(id))
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
