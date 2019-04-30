#if ! defined(FUNCTIONDEFPARSER_HPP)
#define FUNCTIONDEFPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/CountedListParser.hpp"
#include "input/LambdaParser.hpp"
#include "input/TypeParsers.hpp"
#include "util/MemoryManager.hpp"

#include <vector>

namespace scam
{
    class MemoryManager;

    class FunctionDefParser : public ArgParser
    {
        friend class scam::MemoryManager;

        FunctionDefParser()
        {
            MemoryManager & mm = standardMemoryManager;

            name   = mm.make<SymbolParser>();
            lambda = mm.make<LambdaParser>();
        }

        static FunctionDefParser * makeInstance()
        {
            return new FunctionDefParser();
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                ArgParser::mark();

                name->mark();
                lambda->mark();
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
                standardMemoryManager.make<CountedListParser>(any, 2, 99999);

            if ( ! temp->accept(expr) ) {
                return false;
            }

            if ( ! name->accept(temp->get(0)) ) {
                return false;
            }

            if ( ! lambda->accept(expr->nthcdr(0)) ) {
                return false;
            }

            callback(expr);
            return true;
        }

        const ScamSymbol * getName() const
        {
            return dynamic_cast<const ScamSymbol *>(name->getValue());
        }

        const LambdaParser * getLambda() const
        {
            return lambda;
        }

    private:
        SymbolParser * name;
        LambdaParser * lambda;
    };
}

#endif
