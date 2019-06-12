#include "input/ListParser.hpp"

#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
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

    if ( ! isPair(expr) && ! isNull(expr) ) {
        return false;
    }

    if ( isNull(expr) ) {
        callback(expr);
        return true;
    }

    ScamValue current = expr;
    while ( isPair(current) ) {
        ScamValue item = getCar(current);
        current = getCdr(current);
        if ( ! itemParser->accept(item) ) {
            clearValue();
            return false;
        }
        items.push_back(item);
    }

    if ( ! isNull(current) ) {
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
        return makeNothing();
    }
    return items[idx];
}

ListParser * scam::getListOfAnythingParser()
{
    ArgParser * any = standardMemoryManager.make<ArgParser>();
    return standardMemoryManager.make<ListParser>(any);
}
