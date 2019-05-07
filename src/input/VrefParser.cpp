#include "input/VrefParser.hpp"

#include "expr/ScamInteger.hpp"
#include "expr/ScamVector.hpp"
#include "input/SequenceParser.hpp"

using namespace scam;
using namespace std;

VrefParser::VrefParser()
    : ArgParser()
{
    MemoryManager & mm = standardMemoryManager;
    intVal = mm.make<IntegerParser>();
    vecVal = mm.make<VectorParser>();
    parser = mm.make<SequenceParser>(intVal, vecVal);
}

VrefParser * VrefParser::makeInstance()
{
    return new VrefParser;
}

void VrefParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        parser->mark();
    }
}

bool VrefParser::accept(ExprHandle expr)
{
    if ( ! parser->accept(expr) ) {
        return false;
    }

    if ( intVal->getValue()->toInteger() < 0 ) {
        return false;
    }

    callback(expr);
    return true;
}

size_t VrefParser::getIndex() const
{
    return static_cast<size_t>(intVal->getValue()->toInteger());
}

ScamVector * VrefParser::getVector() const
{
    return dynamic_cast<ScamVector *>(vecVal->getValue());
}