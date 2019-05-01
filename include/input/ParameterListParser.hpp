#if ! defined(PARAMETERLISTPARSER_HPP)
#define PARAMETERLISTPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

#include <vector>

namespace scam
{
    class ScamSymbol;

    class ParameterListParser : public ArgParser
    {
        friend class scam::MemoryManager;
        ParameterListParser();
        static ParameterListParser * makeInstance();

    public:
        void mark() const override;
        bool accept(ExprHandle expr) override;
        void clearValue() override;

        size_t size() const;
        const ScamSymbol * get(size_t idx) const;
        const ScamSymbol * getRest() const;

    private:
        SymbolParser      * bare;
        SymbolParser      * sym;
        ListParser        * symList;
        AlternativeParser * parser;

        std::vector<const ScamSymbol *> parameters;
        const ScamSymbol * restParameter;

        bool uniquifyParameters();
    };
}

#endif
