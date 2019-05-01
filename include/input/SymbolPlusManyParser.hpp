#if ! defined(SYMBOLPLUSMANYPARSER_HPP)
#define SYMBOLPLUSMANYPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

namespace scam
{
    class SymbolPlusManyParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;
        SymbolPlusManyParser();
        static SymbolPlusManyParser * makeInstance();

    public:
        void mark() const override;
        bool accept(ExprHandle expr) override;

        ScamEnvKeyType getSymbol() const;
        ExprHandle getForms() const;

    protected:
	void clearValue() override;

    private:
        SymbolParser * sym;
	ExprHandle     forms;
    };
}

#endif
