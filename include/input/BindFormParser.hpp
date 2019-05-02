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
        void mark() const override;
        bool accept(ExprHandle expr) override;

        ScamEnvKeyType getSymbol() const;
        ExprHandle getForm() const;

    private:
        SymbolParser      * sym;
        CountedListParser * parser;
    };
}

#endif
