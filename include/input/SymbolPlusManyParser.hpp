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
        bool accept(ScamValue expr) override;

        ScamValue getSymbol() const;
        ScamValue getForms() const;

    protected:
	void clearValue() override;

    private:
        SymbolParser * sym;
	ScamValue     forms;
    };
}

#endif
