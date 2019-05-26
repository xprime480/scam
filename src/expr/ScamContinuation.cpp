#include "expr/ScamContinuation.hpp"

using namespace scam;
using namespace std;

ScamContinuation::ScamContinuation(Continuation * cont)
    : ScamData(ScamData::Cont)
{
    CONTINUATION(this) = cont;
}

ScamContinuation * ScamContinuation::makeInstance(Continuation * cont)
{
    return new ScamContinuation(cont);
}
