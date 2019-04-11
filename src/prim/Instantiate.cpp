

#include "prim/Instantiate.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamDict.hpp"
#include "prim/Instantiator.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Instantiate::Instantiate()
    : Primitive("instantiate")
{
}

Instantiate * Instantiate::makeInstance()
{
    return new Instantiate();
}

void Instantiate::applyArgs(ScamExpr * args, Continuation * cont)
{
    Instantiator inst(counter);
    ScamExpr * rv = inst.exec(args);
    cont->run(rv);
}

size_t Instantiate::counter { 0 };
