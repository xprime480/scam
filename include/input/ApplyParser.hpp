#if ! defined(APPLYPARSER_HPP)
#define APPLYPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

namespace scam
{
    class MemoryManager;

    class ApplyParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;
        ApplyParser();
        static ApplyParser * makeInstance();

    public:
        void mark() const override;
        bool accept(ScamValue expr) override;

        ScamValue getParsedOp() const;
        ScamValue getArgs() const;

    private:
        CountedListParser * parser;
    };
}

#endif
