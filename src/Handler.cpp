#include "Handler.hpp"

#include "util/GlobalId.hpp"

#include <iostream>

using namespace scam;
using namespace std;

Handler::Handler(const char * id)
    : name(GlobalId::makeName(id))
{
}

Handler * Handler::makeInstance()
{
    return new Handler("Default Handler");
}

ScamValue Handler::handleError(ScamValue err)
{
    return err;
}

string Handler::id() const
{
    return name;
}
