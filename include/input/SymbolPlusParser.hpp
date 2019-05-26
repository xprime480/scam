#if ! defined(SYMBOLPLUSPARSER_HPP)
#define SYMBOLPLUSPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

namespace scam
{
    class SymbolPlusParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;
        SymbolPlusParser();
        static SymbolPlusParser * makeInstance();

    public:
        void mark() const override;
        bool accept(ScamValue expr) override;

        ScamValue getSymbol() const;
        ScamValue getForm() const;

    private:
        SymbolParser   * sym;
        ArgParser      * form;
        SequenceParser * parser;
    };
}

#endif
