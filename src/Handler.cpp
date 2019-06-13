#include "Handler.hpp"

#include "expr/ValueWriter.hpp"

#include <iostream>

using namespace scam;
using namespace std;

ScamValue Handler::handleError(ScamValue err)
{
    return err;
}
