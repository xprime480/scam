#include "form/SpecialForm.hpp"

#include <sstream>

using namespace scam;
using namespace std;

SpecialForm::SpecialForm(string const & name)
    : name(name)
{
}

string SpecialForm::toString() const
{
    stringstream s;
    s << "Special Form " << name;
    return s.str();
}

bool SpecialForm::hasApply() const
{
    return true;
}
