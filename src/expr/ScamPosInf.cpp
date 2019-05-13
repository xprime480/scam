#include "expr/ScamPosInf.hpp"

using namespace scam;
using namespace std;

ScamPosInf::ScamPosInf()
    : ScamSpecialNumeric()
{
}

ScamPosInf * ScamPosInf::makeInstance()
{
    static ScamPosInf inf;
    return &inf;
}

string ScamPosInf::toString() const
{
    static const string repr { "+inf.0" };
    return repr;
}

bool ScamPosInf::isPosInf() const
{
    return true;
}

