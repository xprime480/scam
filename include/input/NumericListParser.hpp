#if ! defined(NUMERICLISTPARSER_HPP)
#define NUMERICLISTPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ExtendedNumericParser.hpp"

#include <cstddef>

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
        void mark() override;
        bool accept(ScamValue expr) override;

        size_t size() const;
        ScamValue get(size_t idx) const;

    private:
        ExtendedNumericParser * num;
        ListParser * parser;
    };
}

#endif
