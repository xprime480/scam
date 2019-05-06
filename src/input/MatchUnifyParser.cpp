#include "input/MatchUnifyParser.hpp"

#include "expr/ExpressionFactory.hpp"
#include "expr/ScamDict.hpp"
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

void MatchUnifyParser::mark() const
{
    if ( ! isMarked() ) {
        ArgParser::mark();
        parser->mark();
    }
}

bool MatchUnifyParser::accept(ExprHandle expr)
{
    ArgParser::clearValue();

    if ( ! parser->accept(expr) ) {
        return false;
    }

    if ( ! match && 3 == parser->size() && ! parser->get(2)->isDict() ) {
        return false;
    }

    callback(expr);
    return true;
}

bool MatchUnifyParser::isMatch() const
{
    return match;
}

ExprHandle MatchUnifyParser::getLhs() const
{
    return parser->get(0);
}

ExprHandle MatchUnifyParser::getRhs() const
{
    return parser->get(1);
}

ScamDict * MatchUnifyParser::getDict() const
{
    if ( ! match && 3 == parser->size() ) {
        return dynamic_cast<ScamDict *>(parser->get(2));
    }
    return ExpressionFactory::makeDict();
}

