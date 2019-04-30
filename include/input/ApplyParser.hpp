#if ! defined(APPLYPARSER_HPP)
#define APPLYPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParser.hpp"
#include "input/CountedListParser.hpp"

namespace scam
{
    class MemoryManager;
    class ScamKeyword;

    class ApplyParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;

        ApplyParser()
            : any(standardMemoryManager.make<ArgParser>())
            , parser(standardMemoryManager.make<CountedListParser>(any, 2, 2))
        {
            clearValue();
        }

        static ApplyParser * makeInstance()
        {
            return new ApplyParser;
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                ArgParser::mark();
                any->mark();
                parser->mark();
            }
        }

        bool accept(ExprHandle expr) override
        {
            ArgParser::clearValue();

            if ( ! parser->accept(expr) ) {
                return false;
            }

            callback(expr);
            return true;
        }

        ExprHandle getParsedOp() const
        {
            if ( 2u == parser->size() ) {
                return parser->get(0);
            }
            return nullptr;
        }

        ExprHandle getArgs() const
        {
            if ( 2u == parser->size() ) {
                return parser->get(1);
            }
            return nullptr;
        }

    private:
        ArgParser         * any;
        CountedListParser * parser;
    };
}

#endif
