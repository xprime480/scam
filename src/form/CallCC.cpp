#include "form/CallCC.hpp"

#include "Continuation.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "form/CallCont.hpp"
#include "input/SingletonParser.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

static const char * myName = "call/cc";

CallCC::CallCC()
    : SpecialForm(myName, applyCallCC)
{
}

CallCC * CallCC::makeInstance()
{
    static CallCC instance;
    return &instance;
}

void scam::applyCallCC(ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine)
{
    SingletonParser * parser = getSingletonOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form)", args, cont);
    }
    else {
        ScamValue body = parser->get();
        Continuation * newCont =
            standardMemoryManager.make<CallCont>(cont, env);
        eval(body, newCont, env);
    }
}
