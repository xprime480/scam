#include "form/ClassMaker.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "input/ClassDefParser.hpp"
#include "util/MemoryManager.hpp"
#include "util/ArgListHelper.hpp"
#include "util/Validations.hpp"

using namespace scam;
using namespace std;

static const char * myName = "class-maker";

ClassMaker::ClassMaker()
    : SpecialForm(myName)
{
}

ClassMaker * ClassMaker::makeInstance()
{
    static ClassMaker instance;
    return &instance;
}

void ClassMaker::apply(ScamValue args, Continuation * cont, Env * env)
{
    ClassDefParser * parser = standardMemoryManager.make<ClassDefParser>();

    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(Base (vars*) methods*)", args, cont);
    }
    else {
        ScamValue cls = ExpressionFactory::makeClass(parser, env);
        cont->run(cls);
    }
}
