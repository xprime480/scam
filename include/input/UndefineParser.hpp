#if ! defined(UNDEFINEPARSER_HPP)
#define UNDEFINEPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

namespace scam
{
    class MemoryManager;

    class UndefineParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;
        UndefineParser();
        static UndefineParser * makeInstance();

    public:
        void mark() override;
        bool accept(ScamValue expr) override;

        ScamValue getSymbol() const;

    private:
        SymbolParser   * sym;
        SequenceParser * parser;
    };
}

#endif
