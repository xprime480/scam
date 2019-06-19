#include "util/GlobalId.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    unsigned long counter { 0 };
}

GlobalId::GlobalId()
 : id(++counter)
{
}

string GlobalId::makeName(const char * tag)
{
    GlobalId next;
    stringstream s;
    s << tag << "{" << next.id << "}";
    return s.str();
}
