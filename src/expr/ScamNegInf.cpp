#include "expr/ScamNegInf.hpp"

using namespace scam;
using namespace std;

ScamNegInf::ScamNegInf()
    : ScamSpecialNumeric()
{
}

ScamNegInf * ScamNegInf::makeInstance()
{
    static ScamNegInf inf;
    return &inf;
}

string ScamNegInf::toString() const
{
    static const string repr { "-inf.0" };
    return repr;
}
