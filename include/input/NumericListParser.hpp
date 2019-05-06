#if ! defined(NUMERICLISTPARSER_HPP)
#define NUMERICLISTPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/TypeParsers.hpp"

namespace scam
{
    class MemoryManager;
    class ListParser;
    class ScamNumeric;

    class NumericListParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;
        NumericListParser();
        static NumericListParser * makeInstance();

    public:
        void mark() const override;
        bool accept(ExprHandle expr) override;

        size_t size() const;
        ScamNumeric * get(size_t idx) const;

    private:
        NumericParser * num;
        ListParser * parser;
    };
}

#endif
