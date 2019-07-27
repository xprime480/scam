#include "form/TemplateData.hpp"

#include "ErrorCategory.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"

#include <sstream>

using namespace scam;
using namespace std;

TemplateData::~TemplateData()
{
}

void TemplateData::getIdentifiers(IDSet & identifiers) const
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

ScamValue TemplateDataLiteral::expandCount(const SyntaxMatchData & data, int n)
{
    return value;
}

string TemplateDataLiteral::identify() const
{
    stringstream s;
    s << writeValue(value);
    return s.str();
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

ScamValue
TemplateDataIdentifier::expandCount(const SyntaxMatchData & data, int n)
{
    ScamValue rv = data.get(identifier, n);
    return rv;
}

void TemplateDataIdentifier::getIdentifiers(IDSet & identifiers) const
{
    identifiers.insert(identifier);
}

string TemplateDataIdentifier::identify() const
{
    return identifier;
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
    if ( ! isMarked() ) {
        TemplateData::mark();
        for ( const auto t : templates ) {
            t->mark();
        }
    }
}

ScamValue TemplateDataList::expand(const SyntaxMatchData & data)
{
    vector<ScamValue> parts;

    for ( const auto t : templates ) {
        ScamValue part = t->expand(data);
        if ( isUnhandledError(part) ) {
            return part;
        }
        if ( part->hasMeta("splice") ) {
            while ( isPair(part) ) {
                parts.push_back(getCar(part));
                part = getCdr(part);
            }
        }
        else {
            parts.push_back(part);
        }
    }

    return makeList(parts);
}

ScamValue TemplateDataList::expandCount(const SyntaxMatchData & data, int n)
{
    vector<ScamValue> parts;

    for ( const auto t : templates ) {
        ScamValue part = t->expandCount(data, n);
        if ( isUnhandledError(part) ) {
            return part;
        }
        parts.push_back(part);
    }

    return makeList(parts);
}

void TemplateDataList::getIdentifiers(IDSet & identifiers) const
{
    for ( const auto t : templates ) {
        t->getIdentifiers(identifiers);
    }
}

string TemplateDataList::identify() const
{
    stringstream s;
    string sep { "" };

    s << "(";
    for ( const auto t : templates ) {
        s << sep << t->identify();
        sep = " ";
    }
    s << ")";

    return s.str();
}


TemplateDataEllipsis::TemplateDataEllipsis(TemplateData * subTemplate)
    : subTemplate(subTemplate)
{
}

TemplateDataEllipsis *
TemplateDataEllipsis::makeInstance(TemplateData * subTemplate)
{
    return new TemplateDataEllipsis(subTemplate);
}

void TemplateDataEllipsis::mark()
{
    if ( ! isMarked() ) {
        TemplateData::mark();
        subTemplate->mark();
    }
}

ScamValue TemplateDataEllipsis::expand(const SyntaxMatchData & data)
{
    set<string> identifiers;
    subTemplate->getIdentifiers(identifiers);

    if ( identifiers.empty() ) {
        return noIdentifiers();
    }

    int repeatCount { -1 };
    for ( const auto & i : identifiers ) {
        if ( data.hasEllipsisId(i) ) {
            int x = data.count(i);
            if ( -1 == repeatCount ) {
                repeatCount = x;
            }
            else if ( x != repeatCount ) {
                return noIdentifiers();
            }
        }
    }

    vector<ScamValue> parts;

    for ( int i = 0 ; i < repeatCount ; ++i ) {
        ScamValue part = subTemplate->expandCount(data, i);
        if ( isUnhandledError(part) ) {
            return part;
        }
        parts.push_back(part);
    }

    ScamValue rv = makeList(parts);
    rv->setMeta("splice", makeNothing());
    return rv;
}

ScamValue TemplateDataEllipsis::expandCount(const SyntaxMatchData & data, int n)
{
    const char * text { "Error: recursive ellipsis template not supported" };
    ScamValue err = makeError(text);
    err->errorCategory() = envCategory;
    return err;
}

void TemplateDataEllipsis::getIdentifiers(IDSet & identifiers) const
{
    subTemplate->getIdentifiers(identifiers);
}

string TemplateDataEllipsis::identify() const
{
    stringstream s;
    s << subTemplate->identify() << " ...";
    return s.str();
}

ScamValue TemplateDataEllipsis::noIdentifiers() const
{
    const char * text { "Error: ellipsis template with no identifiers" };
    ScamValue err = makeError(text);
    err->errorCategory() = envCategory;
    return err;
}
