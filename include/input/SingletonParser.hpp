#if ! defined(SINGELTONPARSER_HPP)
#define SINGELTONPARSER_HPP 1

#include "input/CountedListParser.hpp"

namespace scam
{
    class MemoryManager;

    /**
     * Given an item parser, match a list of exactly 1 item.
     */
    class SingletonParser : public CountedListParser
    {
    private:
        friend class scam::MemoryManager;

        SingletonParser(ArgParser * itemParser)
            : CountedListParser(itemParser, 1, 1)
        {
        }

        static SingletonParser * makeInstance(ArgParser * itemParser)
        {
            return new SingletonParser(itemParser);
        }

    public:
        ExprHandle get() const
        {
            return CountedListParser::get(0);
        }

    };
}

#endif
