#if ! defined(LISTPARSER_HPP)
#define LISTPARSER_HPP 1

#include "input/ArgParser.hpp"

#include <vector>

namespace scam
{
    class MemoryManager;

    /**
     * Given an item parser, match a list of 0.. items.
     */
    class ListParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;

    protected:
        ListParser(ArgParser * itemParser);

    private:
        static ListParser * makeInstance(ArgParser * itemParser);

    public:
        void mark() const override;
        bool accept(ExprHandle expr) override;
        void clearValue() override;

        size_t size() const;
        ExprHandle get(size_t idx) const;

    private:
        ArgParser * itemParser;
        std::vector<ExprHandle> items;
    };

    ListParser * getListOfAnythingParser();
}

#endif
