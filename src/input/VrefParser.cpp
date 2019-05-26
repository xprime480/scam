#include "input/VrefParser.hpp"

#include "expr/ScamNumeric.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/TypePredicates.hpp"
#include "input/SequenceParser.hpp"

using namespace scam;
using namespace std;

VrefParser::VrefParser()
    : ArgParser()
{
    MemoryManager & mm = standardMemoryManager;
    intVal = mm.make<NumericParser>();
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

bool VrefParser::accept(ScamValue expr)
{
    if ( ! parser->accept(expr) ) {
        return false;
    }

    auto val = intVal->getValue();
    if ( ! isInteger(val) || asInteger(val) < 0 ) {
        return false;
    }

    callback(expr);
    return true;
}

size_t VrefParser::getIndex() const
{
    return static_cast<size_t>(asInteger(intVal->getValue()));
}

ScamValue VrefParser::getVector() const
{
    return vecVal->getValue();
}
