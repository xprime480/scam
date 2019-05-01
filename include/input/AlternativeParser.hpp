#if ! defined(ALTERNATIVEPARSER_HPP)
#define ALTERNATIVEPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "util/MemoryManager.hpp"

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
        static AlternativeParser *
        makeInstance(Parsers && ... parsers)
        {
            return new AlternativeParser(parsers...);
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                ArgParser::mark();
                for ( auto p : parsers ) {
                    p->mark();
                }
            }
        }

        bool accept(ExprHandle expr) override
        {
            if ( ! ArgParser::accept(expr) ) {
                return false;
            }

            clearValue();

            for ( size_t idx = 0 ; idx < parsers.size() ; ++idx ) {
                ArgParser * p = parsers[idx];
                if ( p->accept(expr) ) {
                    match = p;
                    break;
                }
            }

            if ( ! match ) {
                return false;
            }

            callback(expr);
            return true;
        }

        void clearValue() override
        {
            ArgParser::clearValue();
            match = nullptr;
        }

        ArgParser * getMatch() const
        {
            return match;
        }

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
