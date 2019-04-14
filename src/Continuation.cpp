
#include "Continuation.hpp"

#include "expr/ScamExpr.hpp"

#include "util/DebugTrace.hpp"

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
    scamTrace("Creating continuation ", this, name);
}

Continuation * Continuation::makeInstance(char const * name)
{
    return new Continuation(name);
}

Continuation::~Continuation()
{
    scamTrace("Deleting continuation ", name);
};

void Continuation::run(ScamExpr * expr)
{
    scamTrace("Executing continuation ", name);
    scamTrace("\tExpr: ", expr->toString());
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
