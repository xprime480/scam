#include "input/DictParser.hpp"

#include "expr/ExpressionFactory.hpp"
#include "input/AlternativeParser.hpp"
#include "input/SequenceParser.hpp"

using namespace scam;
using namespace std;

const ScamKeyword *
DictParser::getOp = ExpressionFactory::makeKeyword(":get", false);

const ScamKeyword *
DictParser::putOp = ExpressionFactory::makeKeyword(":put", false);

const ScamKeyword *
DictParser::lenOp = ExpressionFactory::makeKeyword(":length", false);

const ScamKeyword *
DictParser::remOp = ExpressionFactory::makeKeyword(":remove", false);

const ScamKeyword *
DictParser::hasOp = ExpressionFactory::makeKeyword(":has", false);


DictParser::DictParser()
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

DictParser * DictParser::makeInstance()
{
    return new DictParser;
}

void DictParser::mark() const
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

bool DictParser::accept(ExprHandle expr)
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

void DictParser::callback(ExprHandle expr)
{
    ArgParser::callback(expr);
    matched = dynamic_cast<SequenceParser *>(parser->getMatch());
}

void DictParser::clearValue()
{
    ArgParser::clearValue();
    matched = nullptr;
}

const ScamKeyword * DictParser::getParsedOp() const
{
    if ( matched ) {
        ArgParser * ap = matched->get(0);
        ExprHandle expr = ap ? ap->getValue() : nullptr;
        return dynamic_cast<const ScamKeyword *>(expr);
    }
    return nullptr;
}

ExprHandle DictParser::getOpKey() const
{
    if ( matched ) {
        ArgParser * ap = matched->get(1);
        ExprHandle expr = ap ? ap->getValue() : nullptr;
        return expr;
    }
    return nullptr;
}

ExprHandle DictParser::getOpVal() const
{
    if ( matched ) {
        ArgParser * ap = matched->get(2);
        ExprHandle expr = ap ? ap->getValue() : nullptr;
        return expr;
    }
    return nullptr;
}
