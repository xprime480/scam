#if ! defined(SYNTAXRULE_HPP)
#define SYNTAXRULE_HPP 1

#include "util/ManagedObject.hpp"

#include "ScamFwd.hpp"

#include <set>
#include <string>

namespace scam
{
    class PatternData;
    class SyntaxMatchData;
    class TemplateData;

    class SyntaxRule : public ManagedObject
    {
    private:
        friend class scam::MemoryManager;
        SyntaxRule(ScamValue rule, ScamEngine * engine, ScamValue name);

        static SyntaxRule *
        makeInstance(ScamValue rule, ScamEngine * engine, ScamValue name);

    public:
        void mark() override;

        bool isValid() const;

        bool match(ScamValue args, SyntaxMatchData & data);
        ScamValue expand(const SyntaxMatchData & data);

        std::string identify() const;

    private:
        bool valid;
        PatternData  * pattern;
        TemplateData * templat;
        ScamValue      name;

        std::set<std::string> patternIdentifiers;

        PatternData * parsePattern(ScamValue pat, ScamEngine * engine);
        TemplateData * parseTemplate(ScamValue tem);

        ScamValue invalidPattern(ScamValue pat);
        ScamValue invalidTemplate(ScamValue tem);
    };
}

#endif
