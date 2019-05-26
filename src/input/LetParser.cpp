#include "input/LetParser.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "input/BindFormParser.hpp"
#include "input/CountedListParser.hpp"

using namespace scam;
using namespace std;

LetParser::LetParser()
    : forms(ExpressionFactory::makeNull())
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
        forms->mark();
    }
}

bool LetParser::accept(ScamValue expr)
{
    clearValue();

    CountedListParser * parser = getCountedListOfAnythingParser(1, 99999);
    if ( ! parser->accept(expr) ) {
        return false;
    }

    ScamValue bList = parser->get(0);
    if ( ! isList(bList) ) {
        return false;
    }

    const size_t count = length(bList);
    std::vector<BindFormParser *> tmp;
    for ( size_t idx = 0 ; idx < count ; ++idx ) {
        BindFormParser * b = standardMemoryManager.make<BindFormParser>();
        if ( ! b->accept(nthcar(bList, idx)) ) {
            return false;
        }
        tmp.push_back(b);
    }

    bindings.swap(tmp);
    forms = getCdr(expr);

    callback(expr);
    return true;
}

void LetParser::clearValue()
{
    ArgParser::clearValue();
    bindings.clear();
    forms = ExpressionFactory::makeNull();
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

ScamValue LetParser::getForms() const
{
    return forms;
}
