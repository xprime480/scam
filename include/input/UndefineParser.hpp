#if ! defined(UNDEFINEPARSER_HPP)
#define UNDEFINEPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "expr/ScamSymbol.hpp"
#include "input/SequenceParser.hpp"
#include "input/TypeParsers.hpp"

namespace scam
{
    class MemoryManager;

    class UndefineParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;

        UndefineParser()
        {
            MemoryManager & mm = standardMemoryManager;

            sym    = mm.make<SymbolParser>();
            parser = mm.make<SequenceParser>(sym);

            clearValue();
        }

        static UndefineParser * makeInstance()
        {
            return new UndefineParser;
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                ArgParser::mark();
                sym->mark();
                parser->mark();
            }
        }

        bool accept(ExprHandle expr) override
        {
            ArgParser::clearValue();

            if ( ! parser->accept(expr) ) {
                return false;
            }

            callback(expr);
            return true;
        }

        ScamEnvKeyType getSymbol() const
        {
            return dynamic_cast<ScamEnvKeyType>(sym->getValue());
        }

    private:
        SymbolParser   * sym;
        SequenceParser * parser;
    };
}

#endif
