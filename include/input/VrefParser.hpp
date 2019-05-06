#if ! defined(VREFPARSER_HPP)
#define VREFPARSER_HPP 1

#include "input/ArgParser.hpp"

#include "input/ArgParserFwd.hpp"

namespace scam
{
    class MemoryManager;
    class ScamDict;

    class VrefParser : public ArgParser
    {
    private:
        friend class scam::MemoryManager;
        VrefParser();
        static VrefParser * makeInstance();

    public:
        void mark() const override;
        bool accept(ExprHandle expr) override;

        size_t getIndex() const;
        ScamVector * getVector() const;

    private:
        IntegerParser  * intVal;
        VectorParser   * vecVal;
        SequenceParser * parser;
    };
}

#endif
