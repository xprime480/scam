#include "prim/Trace.hpp"

#include "Continuation.hpp"
#include "expr/ValueWriter.hpp"

#include <iostream>

using namespace scam;
using namespace std;

void scam::applyTrace(ScamValue args,
                      Continuation * cont,
                      ScamEngine * engine)
{
    cerr << writeValue(args) << "\n";
    cont->run(args);
}

