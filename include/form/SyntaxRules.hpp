#if ! defined(SYNTAXRULES_HPP)
#define SYNTAXRULES_HPP 1

#include "ScamFwd.hpp"
#include "form/SyntaxRule.hpp"

#include <map>
#include <set>
#include <string>
#include <vector>

namespace scam
{
    class SyntaxRules
    {
    public:
        SyntaxRules();
        SyntaxRules(ScamEngine * engine,
                    ScamValue name,
                    ScamValue spec,
                    Env * env);

        void mark() const;

        ScamValue getName() const;
        bool isValid() const;

        void applySyntax(ScamValue args,
                         Continuation * cont,
                         Env * env,
                         ScamEngine * engine);

        ScamValue expandSyntax(ScamValue args, ScamEngine * engine);

    private:
        bool valid;
        ScamValue name;
        std::vector<SyntaxRule *> rules;
        std::set<std::string> reserved;
        Env * defined;

        ScamValue extractRules(ScamValue spec, ScamEngine * engine);
        bool extractReserved(ScamValue syms, ScamEngine * engine);
        bool decodeRule(ScamValue rule, ScamEngine * engine);

        ScamValue expand(ScamValue args, ScamEngine * engine);
        SyntaxRule * findSyntaxRule(ScamValue args, SyntaxMatchData & data);

        ScamValue expandWith(ScamValue args,
                             ScamEngine * engine,
                             SyntaxMatchData & data,
                             SyntaxRule * match);

	Env * extendDefinition(Env * base, SyntaxRule * match);
	
        ScamValue badReserved(ScamValue sym);
        ScamValue invalidSyntax(ScamValue rule);
        ScamValue invalidExpansion(ScamValue args);
    };
}

#endif
