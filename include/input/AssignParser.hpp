#if ! defined(ASSIGNPARSER_HPP)
#define ASSIGNPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

namespace scam
{
    class AssignParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;
        AssignParser();
        static AssignParser * makeInstance();

    public:
        void mark() const override;
        bool accept(ExprHandle expr) override;

        ScamEnvKeyType getSymbol() const;
        ExprHandle getForm() const;

    private:
        SymbolParser   * sym;
        ArgParser      * form;
        SequenceParser * parser;
    };
}

#endif
