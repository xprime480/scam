#include "input/ClassDefParser.hpp"

#include "expr/ScamSymbol.hpp"
#include "input/CountedListParser.hpp"
#include "input/FunctionDefParser.hpp"
#include "input/ListParser.hpp"

using namespace scam;
using namespace std;

ClassDefParser::ClassDefParser()
{
    MemoryManager & mm = standardMemoryManager;

    base = mm.make<SymbolParser>();

    SymbolParser * sym = mm.make<SymbolParser>();
    vars = mm.make<ListParser>(sym);
}

ClassDefParser * ClassDefParser::makeInstance()
{
    return new ClassDefParser();
}

void ClassDefParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        base->mark();
        vars->mark();
        for ( const auto m : methods ) {
            m->mark();
        }
    }
}

bool ClassDefParser::accept(ExprHandle expr)
{
    if ( ! ArgParser::accept(expr) ) {
        return false;
    }

    clearValue();

    CountedListParser * temp = getCountedListOfAnythingParser(2, 99999);

    if ( ! temp->accept(expr) ) {
        return false;
    }

    if ( ! base->accept(temp->get(0)) ) {
        return false;
    }
    if ( ! vars->accept(temp->get(1)) ) {
        return false;
    }

    const size_t count = temp->size();
    std::vector<FunctionDefParser *> ms;
    for ( size_t idx = 2 ; idx < count ; ++idx ) {
        FunctionDefParser * p =
            standardMemoryManager.make<FunctionDefParser>();
        if ( ! p->accept(temp->get(idx)) ) {
            return false;
        }
        ms.push_back(p);
    }
    methods.swap(ms);

    callback(expr);
    return true;
}

void ClassDefParser::clearValue()
{
    ArgParser::clearValue();
    methods.clear();
}

const ScamSymbol * ClassDefParser::getBase() const
{
    return dynamic_cast<const ScamSymbol *>(base->getValue());
}

size_t ClassDefParser::getVarCount() const
{
    return vars->size();
}

const ScamSymbol * ClassDefParser::getVar(size_t idx) const
{
    return dynamic_cast<const ScamSymbol *>(vars->get(idx));
}

size_t ClassDefParser::getMethodCount() const
{
    return methods.size();
}

const FunctionDefParser * ClassDefParser::getMethod(size_t idx) const
{
    if ( idx < methods.size() ) {
        return methods[idx];
    }
    return nullptr;
}
