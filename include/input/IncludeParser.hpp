#if ! defined(INCLUDEPARSER_HPP)
#define INCLUDEPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

#include <vector>

namespace scam
{
    class IncludeParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;
        IncludeParser();
        static IncludeParser * makeInstance();

    public:
        void mark() const override;
        bool accept(ExprHandle expr) override;

    public:
        size_t size() const;
        ExprHandle get(size_t idx) const;

    private:
        StringParser * str;
        CountedListParser * parser;
    };
}

#endif
