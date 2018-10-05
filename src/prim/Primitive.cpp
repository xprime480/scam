
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

void Primitive::apply(std::shared_ptr<ScamExpr> const & args,
                      std::shared_ptr<Continuation> cont,
                      Env & env)
{
}

