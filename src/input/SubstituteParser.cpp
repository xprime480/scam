#include "input/SubstituteParser.hpp"

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

void SubstituteParser::mark()
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        parser->mark();
    }
}

bool SubstituteParser::accept(ScamValue expr)
{
    if ( ! parser->accept(expr) ) {
        return false;
    }

    callback(expr);
    return true;
}

ScamValue SubstituteParser::getForm() const
{
    return any->getValue();
}

ScamValue SubstituteParser::getDict() const
{
    return dict->getValue();
}
