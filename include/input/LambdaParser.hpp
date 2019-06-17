#if ! defined(LAMBDAPARSER_HPP)
#define LAMBDAPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

#include <vector>

namespace scam
{
    class MemoryManager;

    class LambdaParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;

    protected:
        LambdaParser();

    private:
        static LambdaParser * makeInstance();

    public:
        void mark() override;
        bool accept(ScamValue expr) override;
        void clearValue() override;

        const ParameterListParser * getArgs() const;
        size_t getFormCount() const;
        ScamValue getForm(size_t idx) const;
        ScamValue getFormList() const;

    private:
        ParameterListParser * formals;
        std::vector<ScamValue> forms;
        ScamValue formList;
    };
}

#endif
