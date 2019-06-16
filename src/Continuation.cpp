#include "Continuation.hpp"

#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    static unsigned counter { 0 };
}

Continuation::Continuation(char const * id, ScamEngine * engine)
    : name(makeName(id))
    , engine(engine)
{
}

Continuation *
Continuation::makeInstance(char const * name, ScamEngine * engine)
{
    return new Continuation(name, engine);
}

Continuation::~Continuation()
{
};

void Continuation::handleValue(ScamValue value)
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
