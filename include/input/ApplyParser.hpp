#if ! defined(APPLYPARSER_HPP)
#define APPLYPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

namespace scam
{
    class MemoryManager;
    class ScamKeyword;

    class ApplyParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;
        ApplyParser();
        static ApplyParser * makeInstance();

    public:
        void mark() const override;
        bool accept(ExprHandle expr) override;

        ExprHandle getParsedOp() const;
        ExprHandle getArgs() const;

    private:
        CountedListParser * parser;
    };
}

#endif
