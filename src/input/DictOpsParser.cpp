#include "input/DictOpsParser.hpp"

#include "expr/ValueFactory.hpp"
#include "input/AlternativeParser.hpp"
#include "input/SequenceParser.hpp"

using namespace scam;
using namespace std;

ScamValue DictOpsParser::getOp = makeKeyword(":get", false);
ScamValue DictOpsParser::putOp = makeKeyword(":put", false);
ScamValue DictOpsParser::lenOp = makeKeyword(":length", false);
ScamValue DictOpsParser::remOp = makeKeyword(":remove", false);
ScamValue DictOpsParser::hasOp = makeKeyword(":has", false);

DictOpsParser::DictOpsParser()
    : ArgParser()
    , matched(nullptr)
{
    MemoryManager & mm = standardMemoryManager;
    key = mm.make<ArgParser>();
    val = mm.make<ArgParser>();

    kwGet = mm.make<KeywordParser>(getOp);
    kwPut = mm.make<KeywordParser>(putOp);
    kwLen = mm.make<KeywordParser>(lenOp);
    kwRem = mm.make<KeywordParser>(remOp);
    kwHas = mm.make<KeywordParser>(hasOp);

    get = mm.make<SequenceParser>(kwGet, key);
    put = mm.make<SequenceParser>(kwPut, key, val);
    len = mm.make<SequenceParser>(kwLen);
    rem = mm.make<SequenceParser>(kwRem, key);
    has = mm.make<SequenceParser>(kwHas, key);

    parser = mm.make<AlternativeParser>(get, put, len, rem, has);
}

DictOpsParser * DictOpsParser::makeInstance()
{
    return new DictOpsParser;
}

void DictOpsParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();

        key->mark();
        val->mark();

        kwGet->mark();
        kwPut->mark();
        kwLen->mark();
        kwRem->mark();
        kwHas->mark();

        get->mark();
        put->mark();
        len->mark();
        rem->mark();
        has->mark();

        parser->mark();
    }
}

bool DictOpsParser::accept(ScamValue expr)
{
    if ( ! ArgParser::accept(expr) ) {
        return false;
    }

    clearValue();

    if ( ! parser->accept(expr) ) {
        return false;
    }

    callback(expr);
    return true;
}

void DictOpsParser::callback(ScamValue expr)
{
    ArgParser::callback(expr);
    matched = dynamic_cast<SequenceParser *>(parser->getMatch());
}

void DictOpsParser::clearValue()
{
    ArgParser::clearValue();
    matched = nullptr;
}

ScamValue DictOpsParser::getParsedOp() const
{
    ScamValue rv = makeNothing();

    if ( matched ) {
        ArgParser * ap = matched->get(0);
        if ( ap ) {
            rv = ap->getValue();
        }
    }
    return rv;
}

ScamValue DictOpsParser::getOpKey() const
{
    ScamValue rv { makeNothing() };
    if ( matched ) {
        ArgParser * ap = matched->get(1);
        if ( ap ) {
            rv = ap->getValue();
        }
    }
    return rv;
}

ScamValue DictOpsParser::getOpVal() const
{
    ScamValue rv { makeNothing() };
    if ( matched ) {
        ArgParser * ap = matched->get(2);
        if ( ap ) {
            rv = ap->getValue();
        }
    }
    return rv;
}
