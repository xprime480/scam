#include "expr/ScamComplex.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamComplex::ScamComplex(double real, double imag)
    : real(real)
    , imag(imag)
{
}

ScamComplex * ScamComplex::makeInstance(double real, double imag)
{
    return new ScamComplex(real, imag);
}

string ScamComplex::toString() const
{
    stringstream s;
    s << "(" << real << " " << imag << ")";
    return s.str();
}

bool ScamComplex::isComplex() const
{
    return true;
}

double ScamComplex::realPart() const
{
    return real;
}

double ScamComplex::imagPart() const
{
    return imag;
}



