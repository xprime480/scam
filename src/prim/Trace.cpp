#include "prim/Trace.hpp"

#include "Continuation.hpp"
#include "expr/ValueWriter.hpp"

#include <iostream>

using namespace scam;
using namespace std;

Trace::Trace()
    : Primitive("trace")
{
}

Trace * Trace::makeInstance()
{
    return new Trace();
}

void Trace::applyArgs(ScamValue args, Continuation * cont)
{
    cerr << writeValue(args) << "\n";
    cont->run(args);
}
