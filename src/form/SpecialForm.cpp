#include "form/SpecialForm.hpp"

#include <sstream>

using namespace scam;
using namespace std;

SpecialForm::SpecialForm(std::string const & name,
                         SfFunction func,
                         ScamEngine * engine,
                         bool managed)
    : ScamData(ScamData::SpecialForm, managed)
{
    SFNAME(this) = name;
    SFFUNC(this) = func;
    SFENGINE(this) = engine;
}
