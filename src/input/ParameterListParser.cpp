#include "input/ParameterListParser.hpp"

#include "expr/ScamSymbol.hpp"
#include "input/AlternativeParser.hpp"
#include "input/ListParser.hpp"

#include <set>
#include <string>

using namespace scam;
using namespace std;

ParameterListParser::ParameterListParser()
    : restParameter(nullptr)
{
    MemoryManager & mm = standardMemoryManager;
    bare    = mm.make<SymbolParser>();
    sym     = mm.make<SymbolParser>();
    symList = mm.make<ListParser>(sym);
    parser  = mm.make<AlternativeParser>(bare, symList);
}

ParameterListParser * ParameterListParser::makeInstance()
{
    return new ParameterListParser();
}

void ParameterListParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();

        bare->mark();
        sym->mark();
        parser->mark();

        for ( const auto & p : parameters ) {
            p->mark();
        }
        if ( restParameter ) {
            restParameter->mark();
        }
    }
}

bool ParameterListParser::accept(ExprHandle expr)
{
    if ( ! ArgParser::accept(expr) ) {
        return false;
    }

    clearValue();

    if ( ! parser->accept(expr) ) {
        return false;
    }

    if ( ! uniquifyParameters() ) {
        return false;
    }

    if ( ! expr->isList() ) {
        restParameter = parameters.back();
    }

    callback(expr);
    return true;
}

void ParameterListParser::clearValue()
{
    ArgParser::clearValue();
    parameters.clear();
    restParameter = nullptr;
}

size_t ParameterListParser::size() const
{
    return parameters.size();
}

const ScamSymbol * ParameterListParser::get(size_t idx) const
{
    if ( idx < parameters.size() ) {
        return parameters[idx];
    }
    return nullptr;
}

const ScamSymbol * ParameterListParser::getRest() const
{
    return restParameter;
}

bool ParameterListParser::uniquifyParameters()
{
    if ( bare == parser->getMatch() ) {

        const ScamSymbol * rest =
            dynamic_cast<const ScamSymbol *>(bare->getValue());
        if ( ! rest ) {
            return false;
        }
        parameters.push_back(rest);
        return true;
    }

    set<string> seen;
    vector<ScamSymbol const *> ps;

    const size_t count = symList->size();
    for ( size_t idx = 0 ; idx < count ; ++idx ) {
        const ScamSymbol * sym =
            dynamic_cast<const ScamSymbol *>(symList->get(idx));
        if ( ! sym ) {
            return false;
        }

        const string name = sym->toString();
        if ( seen.end() != seen.find(name) ) {
            return false;
        }
        seen.insert(name);
        ps.push_back(sym);
    }

    parameters.swap(ps);
    return true;
}
