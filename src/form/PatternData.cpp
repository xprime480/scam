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

bool PatternDataNothing::match(ScamValue & args, SyntaxMatchData & data)
{
    return true;
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

bool PatternDataIdentifier::match(ScamValue & args, SyntaxMatchData & data)
{
    if ( rest ) {
        data.data[identifier] = args;
        args = makeNull();
    }
    else {
        if ( isNull(args) ) {
            return false;
        }

        data.data[identifier] = getCar(args);
        args                  = getCdr(args);
    }

    return true;
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

bool PatternDataSequence::match(ScamValue & args, SyntaxMatchData & data)
{
    for ( auto p : patterns ) {
        if ( ! p->match(args, data) ) {
            return false;
        }
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

bool PatternDataLiteral::match(ScamValue & args, SyntaxMatchData & data)
{
    if ( ! isPair(args) ) {
        return false;
    }

    if ( equals(value, getCar(args)) ) {
        args = getCdr(args);
        return true;
    }

    return false;
}
