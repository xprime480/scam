#include "expr/ScamDict.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"
#include "input/DictParser.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamDict::ScamDict()
{
}

ScamDict::ScamDict(ValVec const & args)
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

void ScamDict::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();
        for ( size_t idx = 0 ; idx < keys.size() ; ++idx ) {
            keys[idx]->mark();
            vals[idx]->mark();
        }
    }
}

string ScamDict::toString() const
{
    stringstream s;
    s << "{";

    for ( size_t idx = 0 ; idx < keys.size() ; ++idx ) {
        s << " " << keys[idx]->toString()
          << " " << vals[idx]->toString();
    }

    if ( keys.size() ) {
        s << " ";
    }
    s << "}";

    return s.str();
}

bool ScamDict::hasApply() const
{
    return true;
}

void ScamDict::apply(ExprHandle args, Continuation * cont, Env * env)
{
    DictParser * parser = standardMemoryManager.make<DictParser>();

    if ( ! parser->accept(args) ) {
        ExprHandle err
            = ExpressionFactory::makeError("Dict expected ':op args...'; ",
                                           "got ",
                                           args->toString());
        cont->run(err);
        return;
    }

    const ScamKeyword * op = parser->getParsedOp();
    ExprHandle rv = nullptr;

    if ( op->equals(DictParser::getOp) ) {
        rv = get(parser->getOpKey());
    }
    else if ( op->equals(DictParser::putOp) ) {
        /* this is potentially UB so revisit this soon!! */
        ExprHandle val = const_cast<ExprHandle>(parser->getOpVal());
        rv = put(parser->getOpKey(), val);
    }
    else if ( op->equals(DictParser::lenOp) ) {
        rv = ExpressionFactory::makeInteger(length());
    }
    else if ( op->equals(DictParser::hasOp) ) {
        const bool b = has(parser->getOpKey());
        rv = ExpressionFactory::makeBoolean(b);
    }
    else if ( op->equals(DictParser::remOp) ) {
        rv = remove(parser->getOpKey());
    }
    else {
        rv = ExpressionFactory::makeError("Unknown dictionary operator: ",
                                          op->toString());
    }

    cont->run(rv);
}

bool ScamDict::truth() const
{
    return true;
}

bool ScamDict::isDict() const
{
    return true;
}

size_t ScamDict::length() const
{
    return keys.size();
}

bool ScamDict::equals(ConstExprHandle expr) const
{
    if ( ! expr->isDict() ) {
        return false;
    }
    ScamDict const * that = dynamic_cast<ScamDict const *>(expr);

    if ( this->keys.size() != that->keys.size() ) {
        return false;
    }

    size_t len = this->length();
    size_t otherIdx = len+1;

    for ( size_t thisIdx = 0 ; thisIdx < len ; ++thisIdx ) {
        ExprHandle myKey = this->keys[thisIdx];
        for ( otherIdx = 0 ; otherIdx < len ; ++otherIdx ) {
            if ( that->keys[otherIdx]->equals(myKey) ) {
                ExprHandle myVal = this->vals[thisIdx];
                if ( ! that->vals[otherIdx]->equals(myVal) ) {
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
    for ( size_t jdx = 0 ; jdx < keys.size() ; ++jdx ) {
        if ( keys[jdx]->equals(key) ) {
            return true;
        }
    }

    return false;
}

ExprHandle ScamDict::get(ExprHandle key) const
{
    for ( size_t jdx = 0 ; jdx < keys.size() ; ++jdx ) {
        if ( keys[jdx]->equals(key) ) {
            return vals[jdx];
        }
    }

    return ExpressionFactory::makeError("Dict key '",
                                        key->toString(),
                                        "' does not exist");
}

ExprHandle ScamDict::put(ExprHandle key, ExprHandle val)
{
    size_t prev = keys.size();
    ExprHandle v = const_cast<ExprHandle>(val);

    for ( size_t jdx = 0 ; jdx < keys.size() ; ++jdx ) {
        if ( keys[jdx]->equals(key) ) {
            prev = jdx;
            break;
        }
    }

    if ( prev >= keys.size() ) {
        keys.push_back(key);
        vals.push_back(v);
    }
    else {
        vals[prev] = v;
    }

    return v;
}

ExprHandle ScamDict::remove(ExprHandle key)
{
    ExprHandle rv = ExpressionFactory::makeNil();

    for ( size_t jdx = 0 ; jdx < keys.size() ; ++jdx ) {
        if ( keys[jdx]->equals(key) ) {
            keys.erase(keys.begin() + jdx);
            rv = vals[jdx];
            vals.erase(vals.begin() + jdx);
            break;
        }
    }

    return rv;
}

KeyVec const & ScamDict::getKeys() const
{
    return keys;
}
