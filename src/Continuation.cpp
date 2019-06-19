#include "Continuation.hpp"

#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "util/GlobalId.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Continuation::Continuation(char const * id, ScamEngine * engine)
    : name(GlobalId::makeName(id))
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
