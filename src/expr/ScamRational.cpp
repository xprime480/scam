#include "expr/ScamRational.hpp"

#include "util/NumericUtils.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamRational::ScamRational(int num, int den, bool exact)
    : ScamReal((double) num / (double) den, exact)
    , num(num / gcd(num, den))
    , den(den / gcd(num, den))
{
}

ScamRational * ScamRational::makeInstance(int num, int den, bool exact)
{
    return new ScamRational(num, den, exact);
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
