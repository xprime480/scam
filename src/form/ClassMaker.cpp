#include "form/ClassMaker.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "input/ClassDefParser.hpp"
#include "util/MemoryManager.hpp"
#include "util/Validations.hpp"

#include "util/DebugTrace.hpp"

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
    scamTrace("ClassMaker::apply", args->toString());

    ClassDefParser * parser = standardMemoryManager.make<ClassDefParser>();

    if ( ! parser->accept(args) ) {
        scamTrace("rejecting the parse");

        ExprHandle err =
            ExpressionFactory::makeError("ClassMaker expected: (Base",
                                         " (vars...) methods...); ",
                                         "got ",
                                         args->toString());
        cont->run(err);
        return;
    }

    scamTrace("class parsed successfully");

    ExprHandle cls = ExpressionFactory::makeClass(parser, env);
    cont->run(cls);
}
