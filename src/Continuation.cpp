#include "Continuation.hpp"

#include "ErrorCategory.hpp"
#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "util/GlobalId.hpp"

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
