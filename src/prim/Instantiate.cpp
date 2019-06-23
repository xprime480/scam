#include "prim/Instantiate.hpp"

#include "Continuation.hpp"
#include "prim/Instantiator.hpp"
#include "util/Parameter.hpp"

using namespace scam;
using namespace std;

namespace
{
    size_t counter { 0 };
}

void scam::applyInstantiate(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name = "instantiate";
    ObjectParameter p0;
    if ( argsToParms(args, engine, name, p0) ) {
        Instantiator inst(counter);
        ScamValue rv = inst.exec(p0.value);
        cont->handleValue(rv);
    }
}
