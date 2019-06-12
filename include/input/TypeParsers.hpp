#if ! defined(TYPEPARSERS_HPP)
#define TYPEPARSERS_HPP 1

#include "input/ArgParser.hpp"

#include "expr/EqualityOps.hpp"
#include "expr/ScamData.hpp"
#include "util/MemoryManager.hpp"

namespace scam
{
    template <unsigned long MatchType>
    class TypeParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;

        TypeParser(ScamValue target = nullptr, bool invert = false)
            : target(target)
            , invert(invert)
        {
        }

        static TypeParser *
        makeInstance(ScamValue target = nullptr, bool invert = false)
        {
            return new TypeParser(target, invert);
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                ArgParser::mark();
                if ( target ) {
                    target->mark();
                }
            }
        }

        bool accept(ScamValue expr) override
        {
            if ( ! ArgParser::accept(expr) ) {
                return false;
            }

            clearValue();

            if ( 0 == (expr->type & MatchType ) ) {
                return false;
            }

            if ( target ) {
                const bool matches = invert ^ equals(target, expr);
                if ( ! matches  ) {
                    return false;
                }
            }

            callback(expr);
            return true;
        }

    private:
        const ScamValue target;
        const bool      invert;
    };

    using NothingParser = TypeParser<ScamData::Nothing>;
    using NullParser    = TypeParser<ScamData::Null>;
    using CharParser    = TypeParser<ScamData::Character>;
    using StringParser  = TypeParser<ScamData::String>;
    using BooleanParser = TypeParser<ScamData::Boolean>;
    using SymbolParser  = TypeParser<ScamData::Symbol>;
    using KeywordParser = TypeParser<ScamData::Keyword>;

    using NumericParser = TypeParser<ScamData::Numeric>;

    using PairParser    = TypeParser<ScamData::Pair>;
    using DictParser    = TypeParser<ScamData::Dict>;
    using VectorParser  = TypeParser<ScamData::Vector>;
}

#endif
