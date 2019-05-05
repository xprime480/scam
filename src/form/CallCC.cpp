#include "form/CallCC.hpp"

#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"
#include "form/CallCont.hpp"
#include "input/SingletonParser.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

static const char * myName = "call/cc";

CallCC::CallCC()
    : SpecialForm(myName)
{
}

CallCC * CallCC::makeInstance()
{
    static CallCC instance;
    return &instance;
}

void CallCC::apply(ExprHandle args, Continuation * cont, Env * env)
{
    SingletonParser * parser = getSingletonOfAnythingParser();
    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(form)", args, cont);
    }
    else {
        ExprHandle body = parser->get();
        Continuation * newCont =
            standardMemoryManager.make<CallCont>(cont, env);
        body->eval(newCont, env);
    }
}
