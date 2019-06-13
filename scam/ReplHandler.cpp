#include "ReplHandler.hpp"

#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"

#include <iostream>

using namespace scam;
using namespace std;

ScamValue ReplHandler::handleError(ScamValue err)
{
    cerr << "\tError detected: " << writeValue(err) << "\n";
    return makeNothing();
}
