#include "form/ClassMaker.hpp"

#include "Continuation.hpp"
#include "expr/ValueFactory.hpp"
#include "input/ClassDefParser.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"
#include "util/Validations.hpp"


using namespace scam;
using namespace std;

static const char * myName = "class-maker";

ClassMaker::ClassMaker()
    : SpecialForm(myName, applyClassMaker)
{
}

ClassMaker * ClassMaker::makeInstance()
{
    static ClassMaker instance;
    return &instance;
}

void scam::applyClassMaker(ScamValue args,
                           Continuation * cont,
                           Env * env,
                           ScamEngine * engine)
{
    ClassDefParser * parser = standardMemoryManager.make<ClassDefParser>();

    if ( ! parser->accept(args) ) {
        failedArgParseMessage(myName, "(Base (vars*) methods*)", args, cont);
    }
    else {
        ScamValue cls = makeClass(parser, env);
        cont->run(cls);
    }
}
