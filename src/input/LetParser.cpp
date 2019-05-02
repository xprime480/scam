#include "input/LetParser.hpp"

#include "expr/ScamExpr.hpp"
#include "input/BindFormParser.hpp"
#include "input/CountedListParser.hpp"

using namespace scam;
using namespace std;

LetParser::LetParser()
    : forms(nullptr)
{
    clearValue();
}

LetParser * LetParser::makeInstance()
{
    return new LetParser;
}

void LetParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        for ( const auto b : bindings ) {
            b->mark();
        }
        if ( forms ) {
            forms->mark();
        }
    }
}

bool LetParser::accept(ExprHandle expr)
{
    clearValue();

    CountedListParser * parser = getCountedListOfAnythingParser(1, 99999);
    if ( ! parser->accept(expr) ) {
        return false;
    }

    ExprHandle bList = parser->get(0);
    if ( ! bList->isList() ) {
        return false;
    }

    const size_t count = bList->length();
    std::vector<BindFormParser *> tmp;
    for ( size_t idx = 0 ; idx < count ; ++idx ) {
        BindFormParser * b = standardMemoryManager.make<BindFormParser>();
        if ( ! b->accept(bList->nthcar(idx)) ) {
            return false;
        }
        tmp.push_back(b);
    }

    bindings.swap(tmp);
    forms = expr->getCdr();

    callback(expr);
    return true;
}

void LetParser::clearValue()
{
    ArgParser::clearValue();
    bindings.clear();
    forms = nullptr;
}

size_t LetParser::getBindingCount() const
{
    return bindings.size();
}

BindFormParser * LetParser::getBinding(size_t idx) const
{
    if ( idx < bindings.size() ) {
        return bindings[idx];
    }
    return nullptr;
}

ExprHandle LetParser::getForms() const
{
    return forms;
}
