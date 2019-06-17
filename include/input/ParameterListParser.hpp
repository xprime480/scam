#if ! defined(PARAMETERLISTPARSER_HPP)
#define PARAMETERLISTPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

#include <vector>

namespace scam
{
    class ParameterListParser : public ArgParser
    {
        friend class scam::MemoryManager;
        ParameterListParser();
        static ParameterListParser * makeInstance();

    public:
        void mark() override;
        bool accept(ScamValue expr) override;
        void clearValue() override;

        size_t size() const;
        ScamValue get(size_t idx) const;
        ScamValue getRest() const;

    private:
        SymbolParser      * bare;
        SymbolParser      * sym;
        ListParser        * symList;
        AlternativeParser * parser;

        std::vector<ScamValue> parameters;
        ScamValue restParameter;

        bool uniquifyParameters();
    };
}

#endif
