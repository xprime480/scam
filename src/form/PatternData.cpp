#include "form/PatternData.hpp"

#include "expr/EqualityOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "form/SyntaxMatchData.hpp"

#include <sstream>

using namespace scam;
using namespace std;

PatternData::PatternData()
    : ellipsis(false)
{
}

PatternData::~PatternData()
{
}

void PatternData::tagAsEllipsis()
{
    ellipsis = true;
}

bool PatternData::isEllipsis() const
{
    return ellipsis;
}

void PatternData::getPatternIds(PatIDSet & patternIds)
{
}

PatternDataNothing * PatternDataNothing::makeInstance()
{
    return new PatternDataNothing;
}

bool PatternDataNothing::match(ScamValue arg, SyntaxMatchData & data)
{
    return false;
}

string PatternDataNothing::identify() const
{
    return "_";
}

PatternDataIdentifier::PatternDataIdentifier(ScamValue identifier, bool rest)
    : identifier(identifier->stringValue())
    , rest(rest)
    , multiple(false)
{
}

PatternDataIdentifier *
PatternDataIdentifier::makeInstance(ScamValue identifier, bool rest)
{
    return new PatternDataIdentifier(identifier, rest);
}

bool PatternDataIdentifier::match(ScamValue arg, SyntaxMatchData & data)
{
    data.add(identifier, multiple, arg);
    return true;
}

void PatternDataIdentifier::tagAsEllipsis()
{
    PatternData::tagAsEllipsis();
    setMultiple();
}

bool PatternDataIdentifier::isRest() const
{
    return rest;
}

void PatternDataIdentifier::setMultiple()
{
    multiple = true;
}

void PatternDataIdentifier::getPatternIds(PatIDSet & patternIds)
{
    patternIds.insert(this);
}

string PatternDataIdentifier::identify() const
{
    stringstream s;
    if ( rest ) {
        s << ". ";
    }
    s << identifier;
    if ( isEllipsis() ) {
        s << " ...";
    }
    return s.str();
}

PatternDataSequence::
PatternDataSequence(const std::vector<PatternData *> & patterns)
    : patterns(patterns)
{
}

PatternDataSequence *
PatternDataSequence::makeInstance(const std::vector<PatternData *> & patterns)
{
    return new PatternDataSequence(patterns);
}

void PatternDataSequence::mark()
{
    if ( ! isMarked() ) {
        PatternData::mark();
        for ( auto p : patterns ) {
            p->mark();
        }
    }
}

bool PatternDataSequence::match(ScamValue arg, SyntaxMatchData & data)
{
    for ( auto p : patterns ) {
        PatternDataNothing * nothing = dynamic_cast<PatternDataNothing *>(p);
        if ( nothing ) {
            continue;
        }

        PatternDataIdentifier * id = dynamic_cast<PatternDataIdentifier *>(p);
        if ( id && id->isRest() ) {
            if ( ! id->match(arg, data) ) {
                return false;
            }
            arg = makeNull();
            continue;
        }

        if ( ! isPair(arg) ) {
            if ( p->isEllipsis() ) {
                break;
            }
            return false;
        }

        if ( p->isEllipsis() ) {
            while ( ! isNull(arg) ) {
                ScamValue head = getCar(arg);
                SyntaxMatchData subData;
                bool matched = p->match(head, subData);
                if ( matched ) {
                    data.append(subData);
                    arg  = getCdr(arg);
                }
                else {
                    break;
                }
            }
        }
        else {
            ScamValue head = getCar(arg);
            arg            = getCdr(arg);

            if ( ! p->match(head, data) ) {
                return false;
            }
        }
    }

    if ( ! isNull(arg) ) {
        return false;
    }

    return true;
}

void PatternDataSequence::tagAsEllipsis()
{
    PatternData::tagAsEllipsis();

    PatIDSet patternIds;
    getPatternIds(patternIds);
    for ( auto id : patternIds ) {
        id->setMultiple();
    }
}

void PatternDataSequence::getPatternIds(PatIDSet & patternIds)
{
    for ( auto p : patterns ) {
        p->getPatternIds(patternIds);
    }
}

string PatternDataSequence::identify() const
{
    stringstream s;
    s << "(";
    string sep = "";
    for ( auto p : patterns ) {
        string i = p->identify();
        s << sep << i;
        sep = " ";
    }
    s << ")";
    if ( isEllipsis() ) {
        s << " ... ";
    }

    return s.str();
}

PatternDataLiteral::PatternDataLiteral(ScamValue value)
    : value(value)
{
}

PatternDataLiteral * PatternDataLiteral::makeInstance(ScamValue value)
{
    return new PatternDataLiteral(value);
}

void PatternDataLiteral::mark()
{
    if ( ! isMarked() ) {
        PatternData::mark();
        value->mark();
    }
}

bool PatternDataLiteral::match(ScamValue arg, SyntaxMatchData & data)
{
    return equals(value, arg);
}

string PatternDataLiteral::identify() const
{
    return writeValue(value);
}
