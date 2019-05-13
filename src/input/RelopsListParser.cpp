#include "input/RelopsListParser.hpp"

#include "expr/ScamString.hpp"
#include "input/AlternativeParser.hpp"
#include "input/ExtendedNumericParser.hpp"
#include "input/ListParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

RelopsListParser::RelopsListParser()
{
    MemoryManager & mm = standardMemoryManager;
    num     = mm.make<ExtendedNumericParser>();
    numList = mm.make<ListParser>(num);
    str     = mm.make<StringParser>();
    strList = mm.make<ListParser>(str);
    parser  = mm.make<AlternativeParser>(numList, strList);
}

RelopsListParser * RelopsListParser::makeInstance()
{
    return new RelopsListParser;
}

void RelopsListParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        parser->mark();
    }
}

bool RelopsListParser::accept(ExprHandle expr)
{
    if ( ! parser->accept(expr) ) {
        return false;
    }

    callback(expr);
    return true;
}

bool RelopsListParser::isNumeric() const
{
    return numList == parser->getMatch();
}

size_t RelopsListParser::size() const
{
    if ( isNumeric() ) {
        return numList->size();
    }
    return strList->size();
}

ExprHandle RelopsListParser::get(size_t idx) const
{
    if ( isNumeric() ) {
        return numList->get(idx);
    }
    return strList->get(idx);
}
