#include "Worker.hpp"

//#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    static unsigned counter { 0 };
}

Worker::Worker(char const * id)
    : name(makeName(id))
{
    //cerr << "Creating worker " << name << "\n";
}

Worker::~Worker()
{
    //cerr << "Deleting worker " << name << "\n";
}

void Worker::run()
{
    //cerr << "Executing worker " << name << "\n";
}

string Worker::id() const
{
    return name;
}

string Worker::makeName(char const * id)
{
    stringstream s;
    s << (++counter) << " {" << id << "}";
    return s.str();
}
