#if ! defined(DICTPARSER_HPP)
#define DICTPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

namespace scam
{
    class MemoryManager;
    class ScamKeyword;

    class DictParser : public ArgParser
    {
    public:
        static const ScamKeyword * getOp;
        static const ScamKeyword * putOp;
        static const ScamKeyword * lenOp;
        static const ScamKeyword * remOp;
        static const ScamKeyword * hasOp;

    private:
        friend class scam::MemoryManager;
        DictParser();
        static DictParser * makeInstance();

    public:
        void mark() const override;
        bool accept(ExprHandle expr) override;
        void callback(ExprHandle expr) override;
        void clearValue() override;

        const ScamKeyword * getParsedOp() const;
        ExprHandle getOpKey() const;
        ExprHandle getOpVal() const;

    private:
        ArgParser * key;
        ArgParser * val;

        KeywordParser * kwGet;
        KeywordParser * kwPut;
        KeywordParser * kwLen;
        KeywordParser * kwRem;
        KeywordParser * kwHas;

        SequenceParser * get;
        SequenceParser * put;
        SequenceParser * len;
        SequenceParser * rem;
        SequenceParser * has;

        AlternativeParser * parser;

        SequenceParser * matched;
    };
}

#endif
