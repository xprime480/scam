#if ! defined(ARGPARSERFWD_HPP)
#define ARGPARSERFWD_HPP 1

#include "input/TypeParsers.hpp"

namespace scam
{
    class AlternativeParser;
    class ApplyParser;
    class ArgParser;
    class BindFormParser;
    class ClassDefParser;
    class CountedListParser;
    class DictParser;
    class FunctionDefParser;
    class LambdaParser;
    class LetParser;
    class ListParser;
    class ParameterListParser;
    class SequenceParser;
    class SingletonParser;
    class SymbolPlusParser;
    class SymbolPlusManyParser;
    class TypeParsers;
    class UndefineParser;

    using DefineParser = SymbolPlusParser;
    using AssignParser = SymbolPlusParser;
    using InstanceParser = SymbolPlusManyParser;
}

#endif
