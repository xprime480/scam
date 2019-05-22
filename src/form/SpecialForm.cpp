#include "form/SpecialForm.hpp"

#include <sstream>

using namespace scam;
using namespace std;

SpecialForm::SpecialForm(string const & name, bool managed)
    : ScamExpr(ScamData::SpecialForm, managed)
{
    STRVAL(data) = name;
}

string SpecialForm::toString() const
{
    stringstream s;
    s << "Special Form " << STRVAL(data);
    return s.str();
}
