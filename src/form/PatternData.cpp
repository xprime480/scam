#include "form/PatternData.hpp"

#include "expr/EqualityOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "form/SyntaxMatchData.hpp"

using namespace scam;
using namespace std;

PatternData::~PatternData()
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

PatternDataIdentifier::PatternDataIdentifier(ScamValue identifier, bool rest)
    : identifier(identifier->stringValue())
    , rest(rest)
{
}

PatternDataIdentifier *
PatternDataIdentifier::makeInstance(ScamValue identifier, bool rest)
{
    return new PatternDataIdentifier(identifier, rest);
}

bool PatternDataIdentifier::match(ScamValue arg, SyntaxMatchData & data)
{
    data.add(identifier, false, arg);
    return true;
}

bool PatternDataIdentifier::isRest() const
{
    return rest;
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
            return false;
        }

        ScamValue head = getCar(arg);
        arg            = getCdr(arg);

        if ( ! p->match(head, data) ) {
            return false;
        }
    }

    if ( ! isNull(arg) ) {
        return false;
    }

    return true;
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
