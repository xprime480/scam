#if ! defined(SEQUENCEPARSER_HPP)
#define SEQUENCEPARSER_HPP 1

#include "input/ArgParser.hpp"

#include <vector>

namespace scam
{
    /**
     * Match a sequence of parsers of arbitrary type.
     */
    class SequenceParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;

        template <typename ... Parsers>
        SequenceParser(Parsers && ... parsers)
        {
            saveParsers(parsers...);
        }

        template <typename ... Parsers>
        static SequenceParser * makeInstance(Parsers && ... parsers)
        {
            return new SequenceParser(parsers...);
        }

    public:
        void mark() const override;
        bool accept(ExprHandle expr) override;

        ArgParser * get(size_t idx) const;

    private:
        std::vector<ArgParser *> parsers;

        template <typename ... Parsers>
        void saveParsers(ArgParser * parser, Parsers && ... rest)
        {
            parsers.push_back(parser);
            saveParsers(rest...);
        }

        void saveParsers()
        {
        }
    };
}

#endif
