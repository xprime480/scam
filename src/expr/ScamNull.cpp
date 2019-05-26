#include "expr/ScamNull.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

ScamNull::ScamNull()
    : ScamExpr(ScamData::Null, false)
{
}

ScamNull * ScamNull::makeInstance()
{
    static ScamNull instance;
    return &instance;
}
