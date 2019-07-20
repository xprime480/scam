#if ! defined(SYNTAXRULE_HPP)
#define SYNTAXRULE_HPP 1

#include "ScamFwd.hpp"

#include <map>

namespace scam
{
    class PatternData;
    class SyntaxMatchData;

    class SyntaxRule
    {
    public:
        SyntaxRule(ScamValue rule, ScamEngine * engine, ScamValue name);

        void mark() const;
        bool isValid() const;

        bool match(ScamValue args, SyntaxMatchData & data);
        ScamValue substitute(const SyntaxMatchData & data);

    private:

        bool valid;
        PatternData * pattern;
        ScamValue     templat;
        ScamValue     name;

        PatternData * parsePattern(ScamValue pat, ScamEngine * engine);
        ScamValue substituteForm(ScamValue form, const SyntaxMatchData & data);

        ScamValue invalidPattern(ScamValue pat);
    };
}

#endif
