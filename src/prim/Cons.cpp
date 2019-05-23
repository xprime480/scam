#include "prim/Cons.hpp"

#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/TypePredicates.hpp"
#include "input/CountedListParser.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

static const char * myName = "cons";

Cons::Cons()
    : Primitive(myName)
{
}

Cons * Cons::makeInstance()
{
    return new Cons();
}

void Cons::applyArgs(ScamValue args, Continuation * cont)
{
    if ( TypePredicates::error(args) ) {
        cont->run(args);
    }

    CountedListParser * parser = getCountedListOfAnythingParser(2, 2);
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form form)", args, cont);
    }
    else {
        ScamValue car = parser->get(0);
        ScamValue cdr = parser->get(1);
        ScamValue cons = ExpressionFactory::makeCons(car, cdr);
        cont->run(cons);
    }
}

