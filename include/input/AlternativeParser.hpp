#if ! defined(ALTERNATIVEPARSER_HPP)
#define ALTERNATIVEPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "util/MemoryManager.hpp"

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

        template <typename ... Rest>
        AlternativeParser(ArgParser * parser, Rest && ... rest)
            : first(parser)
            , rest(scam::standardMemoryManager.make<AlternativeParser>(rest...))
        {
        }

        explicit AlternativeParser(ArgParser * parser)
            : first(parser)
            , rest(nullptr)
        {
        }

        AlternativeParser()
            : first(nullptr)
            , rest(nullptr)
        {
        }

        template <typename ... Rest>
        static AlternativeParser *
        makeInstance(ArgParser * parser, Rest && ... rest)
        {
            return new AlternativeParser(parser, rest...);
        }

        static AlternativeParser * makeInstance(ArgParser * parser)
        {
            return new AlternativeParser(parser);
        }

        static AlternativeParser * makeInstance ()
        {
            return new AlternativeParser;
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                ArgParser::mark();
                if ( first ) {
                    first->mark();
                }
                if ( rest ) {
                    rest->mark();
                }
            }
        }

        bool accept(ExprHandle expr) override
        {
            if ( ! ArgParser::accept(expr) ) {
                return false;
            }

            clearValue();

            if ( ! first ) {
                return false;
            }

            if ( first->accept(expr) ) {
                callback(expr);
                return true;
            }

            if ( rest ) {
                if ( rest->accept(expr) ) {
                    callback(expr);
                    return true;
                }
            }

            return false;
        }

        ArgParser * getMatch() const
        {
            if ( first->getValue() ) {
                return first;
            }
            if ( rest ) {
                return rest->getMatch();
            }
            return nullptr;
        }

    private:
        ArgParser         * first;
        AlternativeParser * rest;
    };
}

#endif
