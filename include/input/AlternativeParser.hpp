#if ! defined(ALTERNATIVEPARSER_HPP)
#define ALTERNATIVEPARSER_HPP 1

#include "input/ArgParser.hpp"

#include <vector>

namespace scam
{
    class MemoryManager;

    /**
     * Accept one of a set of parsers of arbitrary type.  The parsers
     * will be tried in the order they are sent to the constructor.
     */
    class AlternativeParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;

        template <typename ... Parsers>
        AlternativeParser(Parsers && ... parsers)
            : match(nullptr)
        {
            saveParsers(parsers...);
            clearValue();
        }

        template <typename ... Parsers>
        static AlternativeParser * makeInstance(Parsers && ... parsers)
        {
            return new AlternativeParser(parsers...);
        }

    public:
        void mark() override;
        bool accept(ScamValue expr) override;
        void clearValue() override;

        ArgParser * getMatch() const;

    private:
        ArgParser * match;

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
