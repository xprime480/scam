#include "form/Quote.hpp"

#include "Continuation.hpp"
#include "input/SingletonParser.hpp"
#include "util/ArgListHelper.hpp"

using namespace scam;
using namespace std;

static const char * myName = "quote";

Quote::Quote()
    : SpecialForm(myName, applyQuote)
{
}

Quote * Quote::makeInstance()
{
    static Quote quote;
    return &quote;
}

void scam::applyQuote(ScamValue args,
                      Continuation * cont,
                      Env * env,
                      ScamEngine * engine)
{
    SingletonParser * parser = getSingletonOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(expr)", args, cont);
    }
    else {
        cont->run(parser->get());
    }
}
