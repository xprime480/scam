#include "expr/ScamDict.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/TypePredicates.hpp"
#include "input/DictOpsParser.hpp"
#include "util/ArgListHelper.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamDict::ScamDict()
    : ScamExpr(ScamData::Dict)
{
}

ScamDict::ScamDict(ValVec const & args)
    : ScamDict()
{
    ValVec input = args;
    if ( 1 == (input.size() % 2) ) {
        input.push_back(ExpressionFactory::makeNil());
    }

    for ( size_t idx = 0 ; idx < input.size() ; idx += 2 ) {
        ExprHandle key = input[idx];
        ExprHandle val = input[idx+1];
        put(key, val);
    }
}

ScamDict * ScamDict::makeInstance()
{
    return new ScamDict();
}

ScamDict * ScamDict::makeInstance(ValVec const & args)
{
    return new ScamDict(args);
}

void ScamDict::apply(ExprHandle args, Continuation * cont, Env * env)
{
    DictOpsParser * parser = standardMemoryManager.make<DictOpsParser>();

    if ( ! parser->accept(args) ) {
        failedArgParseMessage("dict", "(:op args{0,2})", args, cont);
        return;
    }

    const ScamKeyword * op = parser->getParsedOp();
    ExprHandle rv = nullptr;

    if ( op->equals(DictOpsParser::getOp) ) {
        rv = get(parser->getOpKey());
    }
    else if ( op->equals(DictOpsParser::putOp) ) {
        /* this is potentially UB so revisit this soon!! */
        ExprHandle val = parser->getOpVal();
        rv = put(parser->getOpKey(), val);
    }
    else if ( op->equals(DictOpsParser::lenOp) ) {
        rv = ExpressionFactory::makeInteger(length(), true);
    }
    else if ( op->equals(DictOpsParser::hasOp) ) {
        const bool b = has(parser->getOpKey());
        rv = ExpressionFactory::makeBoolean(b);
    }
    else if ( op->equals(DictOpsParser::remOp) ) {
        rv = remove(parser->getOpKey());
    }
    else {
        rv = ExpressionFactory::makeError("Unknown dictionary operator: ",
                                          ExprWriter::write(op));
    }

    cont->run(rv);
}

size_t ScamDict::length() const
{
    return DICTKEYS(this).size();
}

bool ScamDict::equals(ConstExprHandle expr) const
{
    if ( ! TypePredicates::isDict(expr) ) {
        return false;
    }

    if ( DICTKEYS(this).size() != DICTKEYS(expr).size() ) {
        return false;
    }

    size_t len = length();
    size_t otherIdx = len+1;

    for ( size_t thisIdx = 0 ; thisIdx < len ; ++thisIdx ) {
        ExprHandle myKey = DICTKEYS(this)[thisIdx];
        for ( otherIdx = 0 ; otherIdx < len ; ++otherIdx ) {
            if ( DICTKEYS(expr)[otherIdx]->equals(myKey) ) {
                ExprHandle myVal = DICTVALS(this)[thisIdx];
                if ( ! DICTVALS(expr)[otherIdx]->equals(myVal) ) {
                    return false;
                }
                break;
            }
        }
        if ( otherIdx >= len ) {
            return false;
        }
    }

    return true;

}

bool ScamDict::has(ExprHandle key) const
{
    for ( size_t jdx = 0 ; jdx < DICTKEYS(this).size() ; ++jdx ) {
        if ( DICTKEYS(this)[jdx]->equals(key) ) {
            return true;
        }
    }

    return false;
}

ExprHandle ScamDict::get(ExprHandle key) const
{
    for ( size_t jdx = 0 ; jdx < DICTKEYS(this).size() ; ++jdx ) {
        if ( DICTKEYS(this)[jdx]->equals(key) ) {
            return DICTVALS(this)[jdx];
        }
    }

    return ExpressionFactory::makeError("Dict key '",
                                        ExprWriter::write(key),
                                        "' does not exist");
}

ExprHandle ScamDict::put(ExprHandle key, ExprHandle val)
{
    size_t prev = DICTKEYS(this).size();

    for ( size_t jdx = 0 ; jdx < DICTKEYS(this).size() ; ++jdx ) {
        if ( DICTKEYS(this)[jdx]->equals(key) ) {
            prev = jdx;
            break;
        }
    }

    if ( prev >= DICTKEYS(this).size() ) {
        DICTKEYS(this).push_back(key);
        DICTVALS(this).push_back(val);
    }
    else {
        DICTVALS(this)[prev] = val;
    }

    return val;
}

ExprHandle ScamDict::remove(ExprHandle key)
{
    ExprHandle rv = ExpressionFactory::makeNil();

    for ( size_t jdx = 0 ; jdx < DICTKEYS(this).size() ; ++jdx ) {
        if ( DICTKEYS(this)[jdx]->equals(key) ) {
            DICTKEYS(this).erase(DICTKEYS(this).begin() + jdx);
            rv = DICTVALS(this)[jdx];
            DICTVALS(this).erase(DICTVALS(this).begin() + jdx);
            break;
        }
    }

    return rv;
}

KeyVec const & ScamDict::getKeys() const
{
    return DICTKEYS(this);
}
