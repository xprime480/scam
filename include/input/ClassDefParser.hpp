#if ! defined(CLASSDEFPARSER_HPP)
#define CLASSDEFPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/CountedListParser.hpp"
#include "input/FunctionDefParser.hpp"
#include "input/TypeParsers.hpp"

#include <vector>

namespace scam
{
    class MemoryManager;

    class ClassDefParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;

        ClassDefParser()
        {
            MemoryManager & mm = standardMemoryManager;

            base = mm.make<SymbolParser>();

            SymbolParser * sym = mm.make<SymbolParser>();
            vars = mm.make<ListParser>(sym);
        }

        static ClassDefParser * makeInstance()
        {
            return new ClassDefParser();
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                ArgParser::mark();
                base->mark();
                vars->mark();
                for ( const auto m : methods ) {
                    m->mark();
                }
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

            if ( ! base->accept(temp->get(0)) ) {
                return false;
            }
            if ( ! vars->accept(temp->get(1)) ) {
                return false;
            }

            const size_t count = temp->size();
            std::vector<FunctionDefParser *> ms;
            for ( size_t idx = 2 ; idx < count ; ++idx ) {
                FunctionDefParser * p =
                    standardMemoryManager.make<FunctionDefParser>();
                if ( ! p->accept(temp->get(idx)) ) {
                    return false;
                }
                ms.push_back(p);
            }
            methods.swap(ms);

            callback(expr);
            return true;
        }

        void clearValue() override
        {
            ArgParser::clearValue();
            methods.clear();
        }

        const ScamSymbol * getBase() const
        {
            return dynamic_cast<const ScamSymbol *>(base->getValue());
        }

        size_t getVarCount() const
        {
            return vars->size();
        }

        const ScamSymbol * getVar(size_t idx) const
        {
            return dynamic_cast<const ScamSymbol *>(vars->get(idx));
        }

        size_t getMethodCount() const
        {
            return methods.size();
        }

        const FunctionDefParser * getMethod(size_t idx) const
        {
            if ( idx < methods.size() ) {
                return methods[idx];
            }
            return nullptr;
        }

    private:
        SymbolParser * base;
        ListParser   * vars;

        std::vector<FunctionDefParser *> methods;
    };
}

#endif
