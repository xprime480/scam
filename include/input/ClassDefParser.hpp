#if ! defined(CLASSDEFPARSER_HPP)
#define CLASSDEFPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

#include <vector>

namespace scam
{
    class MemoryManager;
    class ScamSymbol;

    class ClassDefParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;
        ClassDefParser();
        static ClassDefParser * makeInstance();

    public:
        void mark() const override;
        bool accept(ScamValue expr) override;
        void clearValue() override;

        const ScamSymbol * getBase() const;
        size_t getVarCount() const;
        const ScamSymbol * getVar(size_t idx) const;
        size_t getMethodCount() const;
        const FunctionDefParser * getMethod(size_t idx) const;

    private:
        SymbolParser * base;
        ListParser   * vars;

        std::vector<FunctionDefParser *> methods;
    };
}

#endif
