#include "prim/VRef.hpp"

#include "Continuation.hpp"
#include "expr/SequenceOps.hpp"
#include "input/VrefParser.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

void scam::applyVRef(ScamValue args,
                     Continuation * cont,
                     ScamEngine * engine)
{
    static const char * myName = "vref";

    VrefParser * parser = standardMemoryManager.make<VrefParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(index form)", args, cont, engine);
    }
    else {
        size_t idx = parser->getIndex();
        ScamValue vec = parser->getVector();
        cont->handleValue(nthcar(vec, idx));
    }
}
