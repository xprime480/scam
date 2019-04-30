#if ! defined(LAMBDAPARSER_HPP)
#define LAMBDAPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/CountedListParser.hpp"
#include "input/ParameterListParser.hpp"
#include "util/MemoryManager.hpp"

#include <vector>

namespace scam
{
    class MemoryManager;

    class LambdaParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;

    protected:
        LambdaParser()
            : formList(nullptr)
        {
            MemoryManager & mm = standardMemoryManager;

            formals = mm.make<ParameterListParser>();
        }

    private:
        static LambdaParser * makeInstance()
        {
            return new LambdaParser();
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                ArgParser::mark();

                formals->mark();
                for ( const auto & f : forms ) {
                    f->mark();
                }

                formList->mark();
            }
        }

        bool accept(ExprHandle expr) override
        {
            if ( ! ArgParser::accept(expr) ) {
                return false;
            }

            clearValue();

            ArgParser * any = standardMemoryManager.make<ArgParser>();
            CountedListParser * temp =
                standardMemoryManager.make<CountedListParser>(any, 1, 99999);

            if ( ! temp->accept(expr) ) {
                return false;
            }

            if ( ! formals->accept(temp->get(0)) ) {
                return false;
            }

            const size_t count = temp->size();
            for ( size_t idx = 1u ; idx < count ; ++idx ) {
                forms.push_back(temp->get(idx));
            }
            formList = expr->nthcdr(0);

            callback(expr);
            return true;
        }

        void clearValue() override
        {
            ArgParser::clearValue();
            forms.clear();
            formList = nullptr;
        }

        const ParameterListParser * getArgs() const
        {
            return formals;
        }

        size_t getFormCount() const
        {
            return forms.size();
        }

        ExprHandle getForm(size_t idx) const
        {
            if ( idx < forms.size() ) {
                return forms[idx];
            }
            return nullptr;
        }

        ExprHandle getFormList() const
        {
            return formList;
        }

    private:
        ParameterListParser * formals;

        std::vector<ExprHandle> forms;
        ExprHandle formList;
    };
}

#endif
