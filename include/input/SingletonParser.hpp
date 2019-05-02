#if ! defined(SINGELTONPARSER_HPP)
#define SINGELTONPARSER_HPP 1

#include "input/CountedListParser.hpp"

namespace scam
{
    /**
     * Given an item parser, match a list of exactly 1 item.
     */
    class SingletonParser : public CountedListParser
    {
    private:
        friend class scam::MemoryManager;
        SingletonParser(ArgParser * itemParser);
        static SingletonParser * makeInstance(ArgParser * itemParser);

    public:
        ExprHandle get() const;
    };

    SingletonParser * getSingletonOfAnythingParser();
}

#endif
