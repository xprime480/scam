#if ! defined(COUNTEDLISTPARSER_HPP)
#define COUNTEDLISTPARSER_HPP 1

#include "input/ListParser.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    /**
     * Given an item parser and a min,max count, match a list of
     * min..max items.
     */
    class CountedListParser : public ListParser
    {
    private:
        friend class scam::MemoryManager;

    protected:
        CountedListParser(ArgParser * itemParser, size_t min, size_t max);

    private:
        static CountedListParser *
        makeInstance(ArgParser * itemParser, size_t min, size_t max);

    public:
        bool accept(ExprHandle expr) override;

    private:
        const size_t min;
        const size_t max;
    };

    CountedListParser * getCountedListOfAnythingParser(size_t min, size_t max);
}

#endif
