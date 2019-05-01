#include "input/SequenceParser.hpp"

#include "input/ListParser.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

void SequenceParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        for ( auto p : parsers ) {
            p->mark();
        }
    }
}

bool SequenceParser::accept(ExprHandle expr)
{
    if ( ! ArgParser::accept(expr) ) {
        return false;
    }

    clearValue();

    ArgParser * any = standardMemoryManager.make<ArgParser>();
    ListParser * temp = standardMemoryManager.make<ListParser>(any);

    if ( ! temp->accept(expr) ) {
        return false;
    }

    if ( temp->size() != parsers.size() ) {
        return false;
    }

    for ( size_t idx = 0 ; idx < parsers.size() ; ++idx ) {
        ArgParser * p = parsers[idx];
        ExprHandle  e = temp->get(idx);
        if ( ! p->accept(e) ) {
            return false;
        }
    }

    callback(expr);
    return true;
}

ArgParser * SequenceParser::get(size_t idx) const
{
    if ( idx < parsers.size() ) {
        return parsers[idx];
    }
    return nullptr;
}