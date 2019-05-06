#include "input/SubstituteParser.hpp"

#include "expr/ScamDict.hpp"
#include "input/SequenceParser.hpp"

using namespace scam;
using namespace std;

SubstituteParser::SubstituteParser()
    : ArgParser()
{
    MemoryManager & mm = standardMemoryManager;
    any = mm.make<ArgParser>();
    dict = mm.make<DictParser>();
    parser = mm.make<SequenceParser>(any, dict);
}

SubstituteParser * SubstituteParser::makeInstance()
{
    return new SubstituteParser;
}

void SubstituteParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        parser->mark();
    }
}

bool SubstituteParser::accept(ExprHandle expr)
{
    if ( ! parser->accept(expr) ) {
        return false;
    }

    callback(expr);
    return true;
}

ExprHandle SubstituteParser::getForm() const
{
    return any->getValue();
}

ScamDict * SubstituteParser::getDict() const
{
    return dynamic_cast<ScamDict *>(dict->getValue());
}
