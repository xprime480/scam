#include "input/ListParser.hpp"

#include "expr/ScamExpr.hpp"
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

bool ListParser::accept(ExprHandle expr)
{
    if ( ! ArgParser::accept(expr) ) {
        return false;
    }

    clearValue();

    if ( ! expr->isCons() && ! expr->isNil() ) {
        return false;
    }

    if ( expr->isNil() ) {
        callback(expr);
        return true;
    }

    ExprHandle current = expr;
    while ( current->isCons() ) {
        ExprHandle item = current->getCar();
        current = current->getCdr();
        if ( ! itemParser->accept(item) ) {
            clearValue();
            return false;
        }
        items.push_back(item);
    }

    if ( ! current->isNil() ) {
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

ExprHandle ListParser::get(size_t idx) const
{
    if ( idx >= size() ) {
        return nullptr;
    }
    return items[idx];
}

ListParser * scam::getListOfAnythingParser()
{
    ArgParser * any = standardMemoryManager.make<ArgParser>();
    return standardMemoryManager.make<ListParser>(any);
}
