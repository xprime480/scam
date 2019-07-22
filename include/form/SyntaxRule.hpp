#if ! defined(SYNTAXRULE_HPP)
#define SYNTAXRULE_HPP 1

#include "ScamFwd.hpp"

#include <set>
#include <string>

namespace scam
{
    class PatternData;
    class SyntaxMatchData;
    class TemplateData;

    class SyntaxRule
    {
    public:
        SyntaxRule(ScamValue rule, ScamEngine * engine, ScamValue name);

        void mark() const;
        bool isValid() const;

        bool match(ScamValue args, SyntaxMatchData & data);
        ScamValue expand(const SyntaxMatchData & data);

    private:

        bool valid;
        PatternData  * pattern;
        TemplateData * templat;
        ScamValue      name;

        std::set<std::string> patternIdentifiers;

        PatternData * parsePattern(ScamValue pat, ScamEngine * engine);
        TemplateData * parseTemplate(ScamValue tem);

        ScamValue invalidPattern(ScamValue pat);
    };
}

#endif
