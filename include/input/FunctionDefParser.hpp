#if ! defined(FUNCTIONDEFPARSER_HPP)
#define FUNCTIONDEFPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

namespace scam
{
    class FunctionDefParser : public ArgParser
    {
        friend class scam::MemoryManager;
        FunctionDefParser();
        static FunctionDefParser * makeInstance();

    public:
        void mark() override;
        bool accept(ScamValue expr) override;

        ScamValue getName() const;
        LambdaParser * getLambda() const;

    private:
        SymbolParser * name;
        LambdaParser * lambda;
    };
}

#endif
