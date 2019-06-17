#include "input/AlternativeParser.hpp"

using namespace scam;
using namespace std;

void AlternativeParser::mark()
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        for ( auto p : parsers ) {
            p->mark();
        }
    }
}

bool AlternativeParser::accept(ScamValue expr)
{
    if ( ! ArgParser::accept(expr) ) {
        return false;
    }

    clearValue();

    for ( size_t idx = 0 ; idx < parsers.size() ; ++idx ) {
        ArgParser * p = parsers[idx];
        if ( p->accept(expr) ) {
            match = p;
            break;
        }
    }

    if ( ! match ) {
        return false;
    }

    callback(expr);
    return true;
}

void AlternativeParser::clearValue()
{
    ArgParser::clearValue();
    match = nullptr;
}

ArgParser * AlternativeParser::getMatch() const
{
    return match;
}

