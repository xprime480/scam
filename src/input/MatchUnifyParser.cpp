#include "input/MatchUnifyParser.hpp"

#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "input/CountedListParser.hpp"

using namespace scam;
using namespace std;

MatchUnifyParser::MatchUnifyParser(bool match)
    : match(match)
{
    parser = getCountedListOfAnythingParser(2, match ? 2 : 3);
    clearValue();
}

MatchUnifyParser * MatchUnifyParser::makeInstance(bool match)
{
    return new MatchUnifyParser(match);
}

void MatchUnifyParser::mark()
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        parser->mark();
    }
}

bool MatchUnifyParser::accept(ScamValue expr)
{
    ArgParser::clearValue();

    if ( ! parser->accept(expr) ) {
        return false;
    }

    if ( ! match && 3 == parser->size() && ! isDict(parser->get(2)) ) {
        return false;
    }

    callback(expr);
    return true;
}

bool MatchUnifyParser::isMatch() const
{
    return match;
}

ScamValue MatchUnifyParser::getLhs() const
{
    return parser->get(0);
}

ScamValue MatchUnifyParser::getRhs() const
{
    return parser->get(1);
}

ScamValue MatchUnifyParser::getDict() const
{
    if ( ! match && 3 == parser->size() ) {
        return parser->get(2);
    }
    return makeDict();
}
