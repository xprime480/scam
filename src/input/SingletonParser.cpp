#include "input/SingletonParser.hpp"

#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

SingletonParser::SingletonParser(ArgParser * itemParser)
    : CountedListParser(itemParser, 1, 1)
{
}

SingletonParser * SingletonParser::makeInstance(ArgParser * itemParser)
{
    return new SingletonParser(itemParser);
}

ExprHandle SingletonParser::get() const
{
    return CountedListParser::get(0);
}

SingletonParser * scam::getSingletonOfAnythingParser()
{
    ArgParser * any = standardMemoryManager.make<ArgParser>();
    return standardMemoryManager.make<SingletonParser>(any);
}
