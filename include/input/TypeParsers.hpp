#if ! defined(TYPEPARSERS_HPP)
#define TYPEPARSERS_HPP 1

#include "input/ArgParser.hpp"

#include "expr/ExprFwd.hpp"
#include "util/MemoryManager.hpp"

namespace scam
{
    template <typename ScamType>
    class TypeParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;

        TypeParser(const ScamType * target = nullptr, bool invert = false)
            : target(target)
            , invert(invert)
        {
        }

        static TypeParser *
        makeInstance(const ScamType * target = nullptr, bool invert = false)
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

            if ( nullptr == dynamic_cast<const ScamType *>(expr) ) {
                return false;
            }

            if ( target ) {
                const bool matches = invert ^ target->equals(expr);
                if ( ! matches  ) {
                    return false;
                }
            }

            callback(expr);
            return true;
        }

    private:
        const ScamType * target;
        const bool       invert;
    };

    using NullParser    = TypeParser<ScamNull>;
    using NilParser     = TypeParser<ScamNil>;
    using CharParser    = TypeParser<ScamCharacter>;
    using StringParser  = TypeParser<ScamString>;
    using BooleanParser = TypeParser<ScamBoolean>;
    using SymbolParser  = TypeParser<ScamSymbol>;
    using KeywordParser = TypeParser<ScamKeyword>;

    using NumericParser = TypeParser<ScamNumeric>;

    using ConsParser    = TypeParser<ScamCons>;
    using DictParser    = TypeParser<ScamDict>;
    using VectorParser  = TypeParser<ScamVector>;
}

#endif
