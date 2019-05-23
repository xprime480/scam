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

void List::applyArgs(ScamValue args, Continuation * cont)
{
    /** literally anything goes **/
    
    cont->run(args);
}

