#include "form/SpecialForm.hpp"

#include <sstream>

using namespace scam;
using namespace std;

SpecialForm::SpecialForm(string const & name, bool managed)
    : ScamExpr(managed)
    , name(name)
{
    data.type = ScamData::SpecialForm;
}

string SpecialForm::toString() const
{
    stringstream s;
    s << "Special Form " << name;
    return s.str();
}
