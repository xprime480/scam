
#include "prim/Primitive.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Primitive::Primitive(string const & name)
    : name(name)
{
}

string Primitive::toString() const
{
    stringstream s;
    s << "Primitive " << name;
    return s.str();
}

bool Primitive::hasApply() const
{
    return true;
}

void
Primitive::apply(shared_ptr<ScamExpr> const & args, ScamContext const & context)
{
}

