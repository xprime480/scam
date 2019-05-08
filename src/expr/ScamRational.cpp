#include "expr/ScamRational.hpp"

#include "util/NumericUtils.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamRational::ScamRational(int num, int den)
    : ScamReal((double) num / (double) den)
    , num(num / gcd(num, den))
    , den(den / gcd(num, den))
{
}

ScamRational * ScamRational::makeInstance(int num, int den)
{
    return new ScamRational(num, den);
}

string ScamRational::toString() const
{
    stringstream s;
    s << "(" << num << " / " << den << ")";
    return s.str();
}

bool ScamRational::isRational() const
{
    return true;
}
