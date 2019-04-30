#if ! defined(PARAMETERLISTPARSER_HPP)
#define PARAMETERLISTPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "expr/ScamSymbol.hpp"
#include "input/AlternativeParser.hpp"
#include "input/ListParser.hpp"
#include "input/TypeParsers.hpp"
#include "util/MemoryManager.hpp"

#include <set>
#include <string>
#include <vector>

namespace scam
{
    class MemoryManager;

    class ParameterListParser : public ArgParser
    {
        friend class scam::MemoryManager;

        ParameterListParser()
            : restParameter(nullptr)
        {
            MemoryManager & mm = standardMemoryManager;
            bare    = mm.make<SymbolParser>();
            sym     = mm.make<SymbolParser>();
            symList = mm.make<ListParser>(sym);
            parser  = mm.make<AlternativeParser>(bare, symList);
        }

        static ParameterListParser * makeInstance()
        {
            return new ParameterListParser();
        }

    public:
        void mark() const override
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

        bool accept(ExprHandle expr) override
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

        void clearValue() override
        {
            ArgParser::clearValue();
            parameters.clear();
            restParameter = nullptr;
        }

        size_t size() const
        {
            return parameters.size();
        }

        const ScamSymbol * get(size_t idx) const
        {
            if ( idx < parameters.size() ) {
                return parameters[idx];
            }
            return nullptr;
        }

        const ScamSymbol * getRest() const
        {
            return restParameter;
        }

    private:
        SymbolParser      * bare;
        SymbolParser      * sym;
        ListParser        * symList;
        AlternativeParser * parser;

        std::vector<const ScamSymbol *> parameters;
        const ScamSymbol * restParameter;

        bool uniquifyParameters()
        {
            if ( bare == parser->getMatch() ) {
                scamTrace("bare symbol matched");

                const ScamSymbol * rest =
                    dynamic_cast<const ScamSymbol *>(bare->getValue());
                if ( ! rest ) {
                    scamTrace("oops, symbol is nullptr");
                    return false;
                }
                scamTrace("symbol is", rest->toString());
                parameters.push_back(rest);
                return true;
            }

            scamTrace("0 .. n parameters");

            std::set<std::string> seen;
            std::vector<ScamSymbol const *> ps;

            const size_t count = symList->size();
            for ( size_t idx = 0 ; idx < count ; ++idx ) {
                scamTrace("index", idx);
                const ScamSymbol * sym =
                    dynamic_cast<const ScamSymbol *>(symList->get(idx));
                if ( ! sym ) {
                    scamTrace("oops, bad parameter");
                    return false;
                }

                const std::string name = sym->toString();
                scamTrace("name is", name);
                if ( seen.end() != seen.find(name) ) {
                    scamTrace("already detected");
                    return false;
                }
                seen.insert(name);
                ps.push_back(sym);

                scamTrace("now we have", ps.size(), "parameters");
            }

            parameters.swap(ps);
            scamTrace("final count is", parameters.size(), "parameters");
            return true;
        }
    };
}

#endif
