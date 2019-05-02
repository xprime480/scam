#include "form/CallCC.hpp"

#include "Continuation.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/CallCont.hpp"
#include "input/SingletonParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

CallCC::CallCC()
    : SpecialForm("call/cc")
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
        ExprHandle err =
            ExpressionFactory::makeError("call/cc expects exactly 1 parameter",
                                         "; got: ",
                                         args->toString());
        cont->run(err);
    }
    else {
        ExprHandle body = const_cast<ExprHandle>(parser->get());
        Continuation * newCont =
            standardMemoryManager.make<CallCont>(cont, env);
        body->eval(newCont, env);
    }
}
