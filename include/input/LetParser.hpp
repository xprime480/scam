#if ! defined(LETPARSER_HPP)
#define LETPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

#include <vector>

namespace scam
{
    class LetParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;
        LetParser();
        static LetParser * makeInstance();

    public:
        void mark() const override;
        bool accept(ExprHandle expr) override;

    protected:
        void clearValue() override;

    public:
        size_t getBindingCount() const;
        BindFormParser * getBinding(size_t idx) const;
        ExprHandle getForms() const;

    private:
        std::vector<BindFormParser *> bindings;
        ExprHandle forms;
    };
}

#endif
