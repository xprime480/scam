#include "prim/VRef.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/SequenceOps.hpp"
#include "input/VrefParser.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

static const char * myName = "vref";

VRef::VRef()
    : Primitive(myName)
{
}

VRef * VRef::makeInstance()
{
    return new VRef();
}

void VRef::applyArgs(ScamValue args, Continuation * cont)
{
    VrefParser * parser = standardMemoryManager.make<VrefParser>();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form*)", args, cont);
    }
    else {
        size_t idx = parser->getIndex();
        ScamVector * vec = parser->getVector();
        cont->run(nthcar(vec, idx));
    }
}


