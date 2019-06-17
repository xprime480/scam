#if ! defined(BINDFORMPARSER_HPP)
#define BINDFORMPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

namespace scam
{
    class BindFormParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;
        BindFormParser();
        static BindFormParser * makeInstance();

    public:
        void mark() override;
        bool accept(ScamValue expr) override;

        ScamValue getSymbol() const;
        ScamValue getForm() const;

    private:
        SymbolParser      * sym;
        CountedListParser * parser;
    };
}

#endif
