#include "form/ClassMaker.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "input/ClassDefParser.hpp"
#include "util/MemoryManager.hpp"
#include "util/Validations.hpp"

using namespace scam;
using namespace std;

ClassMaker::ClassMaker()
    : SpecialForm("class-maker")
{
}

ClassMaker * ClassMaker::makeInstance()
{
    static ClassMaker instance;
    return &instance;
}

void ClassMaker::apply(ExprHandle args, Continuation * cont, Env * env)
{
    ClassDefParser * parser = standardMemoryManager.make<ClassDefParser>();

    if ( ! parser->accept(args) ) {
        ExprHandle err =
            ExpressionFactory::makeError("ClassMaker expected: (Base",
                                         " (vars...) methods...); ",
                                         "got ",
                                         args->toString());
        cont->run(err);
        return;
    }

    ExprHandle cls = ExpressionFactory::makeClass(parser, env);
    cont->run(cls);
}
