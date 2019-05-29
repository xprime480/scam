#include "prim/List.hpp"

#include "Continuation.hpp"

using namespace scam;
using namespace std;

void scam::applyList(ScamValue args,
                     Continuation * cont,
                     ScamEngine * engine)
{
    /** literally anything goes **/
    cont->run(args);
}


