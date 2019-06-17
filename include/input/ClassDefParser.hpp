#if ! defined(CLASSDEFPARSER_HPP)
#define CLASSDEFPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

#include <vector>

namespace scam
{
    class MemoryManager;

    class ClassDefParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;
        ClassDefParser();
        static ClassDefParser * makeInstance();

    public:
        void mark() override;
        bool accept(ScamValue expr) override;
        void clearValue() override;

        ScamValue getBase() const;
        size_t getVarCount() const;
        ScamValue getVar(size_t idx) const;
        size_t getMethodCount() const;
        const FunctionDefParser * getMethod(size_t idx) const;

    private:
        SymbolParser * base;
        ListParser   * vars;

        std::vector<FunctionDefParser *> methods;
    };
}

#endif
