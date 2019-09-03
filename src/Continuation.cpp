#include "Continuation.hpp"

#include "expr/ScamData.hpp"
#include "util/GlobalId.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Continuation::Continuation(char const * id)
    : name(GlobalId::makeName(id))
{
}

Continuation *
Continuation::makeInstance(char const * name)
{
    return new Continuation(name);
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
