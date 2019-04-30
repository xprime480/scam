#if ! defined(ASSIGNPARSER_HPP)
#define ASSIGNPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "expr/ScamSymbol.hpp"
#include "input/SequenceParser.hpp"
#include "input/TypeParsers.hpp"

namespace scam
{
    class MemoryManager;

    class AssignParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;

        AssignParser()
        {
            MemoryManager & mm = standardMemoryManager;

            sym    = mm.make<SymbolParser>();
            form   = mm.make<ArgParser>();
            parser = mm.make<SequenceParser>(sym, form);

            clearValue();
        }

        static AssignParser * makeInstance()
        {
            return new AssignParser;
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                ArgParser::mark();
                sym->mark();
                form->mark();
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

        ExprHandle getForm() const
        {
            return form->getValue();
        }

    private:
        SymbolParser   * sym;
        ArgParser      * form;
        SequenceParser * parser;
    };
}

#endif
