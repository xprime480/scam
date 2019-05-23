#include "input/ListParser.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/TypePredicates.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

ListParser::ListParser(ArgParser * itemParser)
    : itemParser(itemParser)
{
}

ListParser * ListParser::makeInstance(ArgParser * itemParser)
{
    return new ListParser(itemParser);
}

void ListParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        itemParser->mark();
        for ( const auto & item : items ) {
            item->mark();
        }
    }
}

bool ListParser::accept(ScamValue expr)
{
    if ( ! ArgParser::accept(expr) ) {
        return false;
    }

    clearValue();

    if ( ! isCons(expr) && ! isNil(expr) ) {
        return false;
    }

    if ( isNil(expr) ) {
        callback(expr);
        return true;
    }

    ScamValue current = expr;
    while ( isCons(current) ) {
        ScamValue item = current->getCar();
        current = current->getCdr();
        if ( ! itemParser->accept(item) ) {
            clearValue();
            return false;
        }
        items.push_back(item);
    }

    if ( ! isNil(current) ) {
        if ( ! itemParser->accept(current) )  {
            clearValue();
            return false;
        }
        items.push_back(current);
    }

    callback(expr);
    return true;
}

void ListParser::clearValue()
{
    ArgParser::clearValue();
    items.clear();
}

size_t ListParser::size() const
{
    return items.size();
}

ScamValue ListParser::get(size_t idx) const
{
    if ( idx >= size() ) {
        return ExpressionFactory::makeNull();
    }
    return items[idx];
}

ListParser * scam::getListOfAnythingParser()
{
    ArgParser * any = standardMemoryManager.make<ArgParser>();
    return standardMemoryManager.make<ListParser>(any);
}
