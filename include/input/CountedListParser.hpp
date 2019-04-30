#if ! defined(COUNTEDLISTPARSER_HPP)
#define COUNTEDLISTPARSER_HPP 1

#include "input/ListParser.hpp"

namespace scam
{
    class MemoryManager;

    /**
     * Given an item parser and a min,max count, match a list of
     * min..max items.
     */
    class CountedListParser : public ListParser
    {
    private:
        friend class scam::MemoryManager;

    protected:
        CountedListParser(ArgParser * itemParser, size_t min, size_t max)
            : ListParser(itemParser)
            , min(min)
            , max(max)
        {
        }

    private:
        static CountedListParser *
        makeInstance(ArgParser * itemParser, size_t min, size_t max)
        {
            return new CountedListParser(itemParser, min, max);
        }

    public:
        bool accept(ExprHandle expr) override
        {
            if ( ! ListParser::accept(expr) ) {
                return false;
            }

            const size_t count = size();
            if ( count < min || count > max ) {
                clearValue();
                return false;
            }

            return true;
        }

    private:
        const size_t min;
        const size_t max;
    };
}

#endif
