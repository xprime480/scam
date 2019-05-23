#include "prim/CarCdr.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"
#include "input/SingletonParser.hpp"
#include "input/TypeParsers.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

CarCdr::CarCdr(char const * name)
    : Primitive(name)
{
}

void CarCdr::applyArgs(ScamValue args, Continuation * cont)
{
    if ( TypePredicates::error(args) ) {
        cont->run(args);
        return;
    }

    ConsParser * cp = standardMemoryManager.make<ConsParser>();
    SingletonParser * parser = standardMemoryManager.make<SingletonParser>(cp);
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(STRVAL(this).c_str(), "(a-cons)", args, cont);
        return;
    }

    finish(parser->get(), cont);
}
