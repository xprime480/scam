#if ! defined(SUBSTITUTEPARSER_HPP)
#define SUBSTITUTEPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

namespace scam
{
    class MemoryManager;
    class ScamDict;

    class SubstituteParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;
        SubstituteParser();
        static SubstituteParser * makeInstance();

    public:
        void mark() const override;
        bool accept(ScamValue expr) override;

        ScamValue getForm() const;
        ScamValue getDict() const;

    private:
        ArgParser * any;
        DictParser * dict;
        SequenceParser * parser;
    };
}

#endif
