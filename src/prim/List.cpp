
#include "prim/List.hpp"

#include "Continuation.hpp"

using namespace scam;
using namespace std;

List::List()
    : Primitive("list")
{
}

List * List::makeInstance()
{
    return new List();
}

void List::applyArgs(ScamExpr * args, Continuation * cont)
{
    cont->run(args);
}

