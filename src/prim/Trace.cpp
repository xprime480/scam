#include "prim/Trace.hpp"

#include "Continuation.hpp"
#include "expr/ExprWriter.hpp"

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

void Trace::applyArgs(ExprHandle args, Continuation * cont)
{
    cerr << ExprWriter::write(args) << "\n";
    cont->run(args);
}
