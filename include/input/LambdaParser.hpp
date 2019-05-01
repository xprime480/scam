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
        void mark() const override;
        bool accept(ExprHandle expr) override;
        void clearValue() override;

        const ParameterListParser * getArgs() const;
        size_t getFormCount() const;
        ExprHandle getForm(size_t idx) const;
        ExprHandle getFormList() const;

    private:
        ParameterListParser * formals;
        std::vector<ExprHandle> forms;
        ExprHandle formList;
    };
}

#endif
