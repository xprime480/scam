#include "prim/Instantiate.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamDict.hpp"
#include "input/SingletonParser.hpp"
#include "prim/Instantiator.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

static const char * myName = "instantiate";

Instantiate::Instantiate()
    : Primitive(myName)
{
}

Instantiate * Instantiate::makeInstance()
{
    return new Instantiate();
}

void Instantiate::applyArgs(ScamValue args, Continuation * cont)
{
    SingletonParser * parser = getSingletonOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form)", args, cont);
    }

    Instantiator inst(counter);
    ScamValue rv = inst.exec(parser);
    cont->run(rv);
}

size_t Instantiate::counter { 0 };
