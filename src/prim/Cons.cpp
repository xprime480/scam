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

void Cons::applyArgs(ExprHandle args, Continuation * cont)
{
    if ( TypePredicates::error(args) ) {
        cont->run(args);
    }

    CountedListParser * parser = getCountedListOfAnythingParser(2, 2);
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form form)", args, cont);
    }
    else {
        ExprHandle car = parser->get(0);
        ExprHandle cdr = parser->get(1);
        ExprHandle cons = ExpressionFactory::makeCons(car, cdr);
        cont->run(cons);
    }
}

