#if ! defined(SYNTAXRULES_HPP)
#define SYNTAXRULES_HPP 1

#include "ScamFwd.hpp"
#include "util/LambdaDef.hpp"

#include <map>
#include <string>
#include <vector>

namespace scam
{
    class SyntaxRules
    {
    public:
        SyntaxRules();
        SyntaxRules(ScamEngine * engine, ScamValue symbol, ScamValue spec);

        void mark() const;

        ScamValue getName() const;

        void applySyntax(ScamValue args,
                         Continuation * cont,
                         Env * env,
                         ScamEngine * engine);

    private:
        bool valid;
        ScamValue name;
        std::vector<LambdaDef> defs;

        using FormalsToActuals = std::map<std::string, ScamValue>;

        ScamValue extractRules(ScamValue spec, ScamEngine * engine);
        bool decodeRule(ScamValue rule, ScamEngine * engine);

        LambdaDef findSyntax(ScamValue args);
        ScamValue substitute(const LambdaDef & lambda, ScamValue actuals);
        ScamValue substituteForm(ScamValue form, const FormalsToActuals & f2a);

        ScamValue invalidSyntax(ScamValue rule);
        ScamValue invalidExpansion(ScamValue args);
        ScamValue conformalError(ScamValue formals, ScamValue actuals);

    };
}

#endif
