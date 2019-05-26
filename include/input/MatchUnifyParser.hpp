#if ! defined(MATCHUNIFYPARSER_HPP)
#define MATCHUNIFYPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

namespace scam
{
    class MatchUnifyParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;
        explicit MatchUnifyParser(bool match);
        static MatchUnifyParser * makeInstance(bool match);

    public:
        void mark() const override;
        bool accept(ScamValue expr) override;

        bool isMatch() const;
        ScamValue getLhs() const;
        ScamValue getRhs() const;
        ScamValue getDict() const;

    private:
        const bool          match;
        CountedListParser * parser;
    };
}

#endif
