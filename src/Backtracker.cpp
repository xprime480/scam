
#include "Backtracker.hpp"

#include "expr/ScamExpr.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    static unsigned counter { 0 };
}

Backtracker::Backtracker(char const * id)
    : name(makeName(id))
{
    //    cerr << "Creating backtracker " << name << "\n";
}

Backtracker::~Backtracker()
{
    //    cerr << "Deleting backtracker " << name << "\n";
};

void Backtracker::run(ContHandle cont)
{
    //    cerr << "Executing backtracker " << name << "\n";
}

string Backtracker::id() const
{
    return name;
}

string Backtracker::makeName(char const * id)
{
    stringstream s;
    s << (++counter) << " {" << id << "}";
    return s.str();
}
