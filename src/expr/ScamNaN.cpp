#include "expr/ScamNaN.hpp"

using namespace scam;
using namespace std;

ScamNaN::ScamNaN()
    : ScamSpecialNumeric()
{
}

ScamNaN * ScamNaN::makeInstance()
{
    static ScamNaN nan;
    return &nan;
}

string ScamNaN::toString() const
{
    static const string repr { "+nan.0" };
    return repr;
}

bool ScamNaN::isNaN() const
{
    return true;
}
