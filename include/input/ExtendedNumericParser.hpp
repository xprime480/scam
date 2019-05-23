#if ! defined(EXTENDEDNUMERICPARSER_HPP)
#define EXTENDEDNUMERICPARSER_HPP 1

#include "input/ArgParser.hpp"

namespace scam
{
    class MemoryManager;

    class ExtendedNumericParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;
        ExtendedNumericParser();
        static ExtendedNumericParser * makeInstance();

    public:
        bool accept(ScamValue expr) override;
    };
}

#endif
