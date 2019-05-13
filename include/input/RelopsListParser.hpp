#if ! defined(RELOPSLISTPARSER_HPP)
#define RELOPSLISTPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"
#include "input/TypeParsers.hpp"

namespace scam
{
    class MemoryManager;
    class ListParser;

    class RelopsListParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;
        RelopsListParser();
        static RelopsListParser * makeInstance();

    public:
        void mark() const override;
        bool accept(ExprHandle expr) override;

        bool isNumeric() const;
        size_t size() const;
        ScamExpr * get(size_t idx) const;

    private:
        ExtendedNumericParser * num;
        StringParser * str;
        ListParser * numList;
        ListParser * strList;
        AlternativeParser * parser;
    };
}

#endif
