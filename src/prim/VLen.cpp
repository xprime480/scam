#include "prim/VLen.hpp"

#include "Continuation.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "input/SingletonParser.hpp"
#include "input/TypeParsers.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

void scam::applyVLen(ScamValue args,
                     Continuation * cont,
                     ScamEngine * engine)
{
    static const char * myName = "vlen";

    VectorParser * vec =  standardMemoryManager.make<VectorParser>();
    SingletonParser * parser
        = standardMemoryManager.make<SingletonParser>(vec);

    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(vec)", args, cont);
    }
    else {
        size_t len = length(parser->get());
        ScamValue val = makeInteger(len, true);
        cont->run(val);
    }
}
