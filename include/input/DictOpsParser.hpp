#if ! defined(DICTOPSPARSER_HPP)
#define DICTOPSPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

namespace scam
{
    class MemoryManager;
    class ScamKeyword;

    class DictOpsParser : public ArgParser
    {
    public:
        static const ScamKeyword * getOp;
        static const ScamKeyword * putOp;
        static const ScamKeyword * lenOp;
        static const ScamKeyword * remOp;
        static const ScamKeyword * hasOp;

    private:
        friend class scam::MemoryManager;
        DictOpsParser();
        static DictOpsParser * makeInstance();

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
