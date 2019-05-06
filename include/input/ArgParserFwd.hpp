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
    class DictOpsParser;
    class FunctionDefParser;
    class IncludeParser;
    class LambdaParser;
    class LetParser;
    class ListParser;
    class MatchUnifyParser;
    class NumericListParser;
    class ParameterListParser;
    class RelopsListParser;
    class SequenceParser;
    class SingletonParser;
    class SubstituteParser;
    class SymbolPlusManyParser;
    class SymbolPlusParser;
    class TypeParsers;
    class UndefineParser;
    class VrefParser;

    using DefineParser = SymbolPlusParser;
    using AssignParser = SymbolPlusParser;
    using InstanceParser = SymbolPlusManyParser;


}

#endif
