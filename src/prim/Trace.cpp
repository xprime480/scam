#include "prim/Trace.hpp"

#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"

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
    cerr << args->toString() << "\n";
    cont->run(args);
}
