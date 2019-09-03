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

        SyntaxRule(ScamValue rule,
                   ScamValue name,
                   const std::set<std::string> & reserved);

        static SyntaxRule *
        makeInstance(ScamValue rule,
                     ScamValue name,
                     const std::set<std::string> & reserved);

    public:
        ~SyntaxRule();

        void mark() override;

        bool isValid() const;

        bool match(ScamValue args, SyntaxMatchData & data);
        ScamValue expand(const SyntaxMatchData & data);

        std::set<ScamValue> getFreeSymbols() const;

        std::string identify() const;

    private:
        bool valid;
        PatternData  * pattern;
        TemplateData * templat;
        ScamValue      name;
        std::set<ScamValue> freeSymbols;

        std::set<std::string> patternIdentifiers;

        PatternData * parsePattern(ScamValue pat,
                                   const std::set<std::string> & reserved);

        TemplateData * parseTemplate(ScamValue tem);

        ScamValue invalidPattern(ScamValue pat);
        ScamValue invalidTemplate(ScamValue tem);
    };
}

#endif
