#include "form/TemplateData.hpp"

#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

#include <vector>

using namespace scam;
using namespace std;

TemplateData::~TemplateData()
{
}

TemplateDataLiteral::TemplateDataLiteral(ScamValue value)
    : value(value)
{
}

TemplateDataLiteral * TemplateDataLiteral::makeInstance(ScamValue value)
{
    return new TemplateDataLiteral(value);
}

void TemplateDataLiteral::mark()
{
    if ( ! isMarked() ) {
        TemplateData::mark();
        value->mark();
    }
}

ScamValue TemplateDataLiteral::expand(const SyntaxMatchData & data)
{
    return value;
}

TemplateDataIdentifier::TemplateDataIdentifier(ScamValue value)
    : identifier(value->stringValue())
{
}

TemplateDataIdentifier * TemplateDataIdentifier::makeInstance(ScamValue value)
{
    return new TemplateDataIdentifier(value);
}

ScamValue TemplateDataIdentifier::expand(const SyntaxMatchData & data)
{
    ScamValue rv = data.get(identifier, 0);
    return rv;
}

TemplateDataList::TemplateDataList(std::vector<TemplateData *> templates)
    : templates(templates)
{
}

TemplateDataList *
TemplateDataList::makeInstance(std::vector<TemplateData *> templates)
{
    return new TemplateDataList(templates);
}

void TemplateDataList::mark()
{
    for ( const auto t : templates ) {
        t->mark();
    }
}

ScamValue TemplateDataList::expand(const SyntaxMatchData & data)
{
    vector<ScamValue> parts;

    for ( const auto t : templates ) {
        ScamValue part = t->expand(data);
        parts.push_back(part);
    }

    return makeList(parts);
}
