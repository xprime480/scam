#include "Continuation.hpp"

#include "expr/ScamExpr.hpp"

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
}

Continuation * Continuation::makeInstance(char const * name)
{
    return new Continuation(name);
}

Continuation::~Continuation()
{
};

void Continuation::run(ExprHandle expr)
{
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
