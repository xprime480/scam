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
        void mark() override;
        bool accept(ScamValue expr) override;

        size_t getIndex() const;
        ScamValue getVector() const;

    private:
        NumericParser  * intVal;
        VectorParser   * vecVal;
        SequenceParser * parser;
    };
}

#endif
