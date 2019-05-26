#include "expr/ScamNil.hpp"

#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"

using namespace scam;
using namespace std;

ScamNil::ScamNil()
    : ScamData(ScamData::Nil, false)
{
}

ScamNil * ScamNil::makeInstance()
{
    static ScamNil nil;
    return &nil;
}
