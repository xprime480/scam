#include "input/DictOpsParser.hpp"

#include "expr/ExpressionFactory.hpp"
#include "input/AlternativeParser.hpp"
#include "input/SequenceParser.hpp"

using namespace scam;
using namespace std;

const ScamKeyword *
DictOpsParser::getOp = ExpressionFactory::makeKeyword(":get", false);

const ScamKeyword *
DictOpsParser::putOp = ExpressionFactory::makeKeyword(":put", false);

const ScamKeyword *
DictOpsParser::lenOp = ExpressionFactory::makeKeyword(":length", false);

const ScamKeyword *
DictOpsParser::remOp = ExpressionFactory::makeKeyword(":remove", false);

const ScamKeyword *
DictOpsParser::hasOp = ExpressionFactory::makeKeyword(":has", false);


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

bool DictOpsParser::accept(ExprHandle expr)
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

void DictOpsParser::callback(ExprHandle expr)
{
    ArgParser::callback(expr);
    matched = dynamic_cast<SequenceParser *>(parser->getMatch());
}

void DictOpsParser::clearValue()
{
    ArgParser::clearValue();
    matched = nullptr;
}

const ScamKeyword * DictOpsParser::getParsedOp() const
{
    if ( matched ) {
        ArgParser * ap = matched->get(0);
        ExprHandle expr = ap ? ap->getValue() : nullptr;
        return dynamic_cast<const ScamKeyword *>(expr);
    }
    return nullptr;
}

ExprHandle DictOpsParser::getOpKey() const
{
    if ( matched ) {
        ArgParser * ap = matched->get(1);
        ExprHandle expr = ap ? ap->getValue() : nullptr;
        return expr;
    }
    return nullptr;
}

ExprHandle DictOpsParser::getOpVal() const
{
    if ( matched ) {
        ArgParser * ap = matched->get(2);
        ExprHandle expr = ap ? ap->getValue() : nullptr;
        return expr;
    }
    return nullptr;
}
