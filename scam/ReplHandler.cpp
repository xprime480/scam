#include "ReplHandler.hpp"

#include "value/ValueFactory.hpp"
#include "value/ValueWriter.hpp"

#include <iostream>

using namespace scam;
using namespace std;

ReplHandler::ReplHandler()
    : Handler("Repl")
{
}

ReplHandler * ReplHandler::makeInstance()
{
    return new ReplHandler;
}

ScamValue ReplHandler::handleError(ScamValue err)
{
    cerr << "\tError detected: " << writeValue(err) << "\n";
    return makeNothing();
}
