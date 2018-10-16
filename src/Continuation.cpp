
#include "Continuation.hpp"

#include "expr/ScamExpr.hpp"

//#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    static unsigned counter { 0 };
}

Continuation::Continuation(char const * id)
    : name(makeName(id))
{
    //cerr << "Creating continuation " << name << "\n";
}

Continuation::~Continuation()
{
    //cerr << "Deleting continuation " << name << "\n";
};

void Continuation::run(ScamExpr * expr)
{
    //cerr << "Executing continuation " << name << "\n";
}

string Continuation::id() const
{
    return name;
}

string Continuation::makeName(char const * id)
{
    stringstream s;
    s << (++counter) << " {" << id << "}";
    return s.str();
}
