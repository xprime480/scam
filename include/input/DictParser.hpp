#if ! defined(DICTPARSER_HPP)
#define DICTPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "expr/ScamKeyword.hpp"
#include "input/AlternativeParser.hpp"
#include "input/ArgParser.hpp"
#include "input/SequenceParser.hpp"
#include "input/TypeParsers.hpp"
#include "util/MemoryManager.hpp"

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

        DictParser()
            : ArgParser()
            , matched(nullptr)
        {
            MemoryManager & mm = standardMemoryManager;
            key = mm.make<ArgParser>();
            val = mm.make<ArgParser>();

            kwGet = mm.make<KeywordParser>(getOp);
            kwPut = mm.make<KeywordParser>(putOp);
            kwLen = mm.make<KeywordParser>(lenOp);
            kwRem = mm.make<KeywordParser>(remOp);
            kwHas = mm.make<KeywordParser>(hasOp);

            get = mm.make<SequenceParser>(kwGet, key);
            put = mm.make<SequenceParser>(kwPut, key, val);
            len = mm.make<SequenceParser>(kwLen);
            rem = mm.make<SequenceParser>(kwRem, key);
            has = mm.make<SequenceParser>(kwHas, key);

            parser = mm.make<AlternativeParser>(get, put, len, rem, has);
        }

        static DictParser * makeInstance()
        {
            return new DictParser;
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                ArgParser::mark();

                key->mark();
                val->mark();

                kwGet->mark();
                kwPut->mark();
                kwLen->mark();
                kwRem->mark();
                kwHas->mark();

                get->mark();
                put->mark();
                len->mark();
                rem->mark();
                has->mark();

                parser->mark();
            }
        }

        bool accept(ExprHandle expr) override
        {
            if ( ! ArgParser::accept(expr) ) {
                return false;
            }

            clearValue();

            if ( ! parser->accept(expr) ) {
                return false;
            }

            callback(expr);
            return true;
        }

        void callback(ExprHandle expr) override
        {
            ArgParser::callback(expr);
            matched = dynamic_cast<SequenceParser *>(parser->getMatch());
        }

        void clearValue() override
        {
            ArgParser::clearValue();
            matched = nullptr;
        }

        const ScamKeyword * getParsedOp() const
        {
            if ( matched ) {
                ArgParser * ap = matched->get(0);
                ExprHandle expr = ap ? ap->getValue() : nullptr;
                return dynamic_cast<const ScamKeyword *>(expr);
            }
            return nullptr;
        }

        ExprHandle getOpKey() const
        {
            if ( matched ) {
                ArgParser * ap = matched->get(1);
                ExprHandle expr = ap ? ap->getValue() : nullptr;
                return expr;
            }
            return nullptr;
        }

        ExprHandle getOpVal() const
        {
            if ( matched ) {
                ArgParser * ap = matched->get(2);
                ExprHandle expr = ap ? ap->getValue() : nullptr;
                return expr;
            }
            return nullptr;
        }

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
