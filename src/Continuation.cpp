#include "Continuation.hpp"

#include "ErrorCategory.hpp"
#include "util/GlobalId.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Continuation::Continuation(char const * id)
    : name(GlobalId::makeName(id))
{
}

Continuation *
Continuation::makeInstance(char const * name)
{
    return new Continuation(name);
}

Continuation::~Continuation()
{
};

void Continuation::handleValue(ScamValue value)
{
}

void Continuation::handleMultipleValues(ScamValue value)
{
    ScamValue rv = makeBoolean(false);

    if ( isMultiple(value) ) {
        ScamData::VectorData & values = value->multipleValues();
        if ( ! values.empty() ) {
            rv = values[0];
        }
    }
    else {
        rv = makeError("Multiple-value continuation received %{0}", value);
        rv->errorCategory() = valuesCategory;
    }

    handleValue(rv);
}

string Continuation::id() const
{
    return name;
}
