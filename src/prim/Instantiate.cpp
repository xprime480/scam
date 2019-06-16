#include "prim/Instantiate.hpp"

#include "Continuation.hpp"
#include "input/SingletonParser.hpp"
#include "prim/Instantiator.hpp"
#include "util/ArgListHelper.hpp"

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
    static const char * myName = "instantiate";

    SingletonParser * parser = getSingletonOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form)", args, cont, engine);
        return;
    }

    Instantiator inst(counter);
    ScamValue rv = inst.exec(parser);
    cont->handleValue(rv);
}
