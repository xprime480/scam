#include "expr/ScamDict.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    static const ScamExpr * getOp =
        ExpressionFactory::makeKeyword(":get", false);
    static const ScamExpr * putOp =
        ExpressionFactory::makeKeyword(":put", false);
    static const ScamExpr * lenOp =
        ExpressionFactory::makeKeyword(":length", false);
    static const ScamExpr * remOp =
        ExpressionFactory::makeKeyword(":remove", false);
    static const ScamExpr * hasOp =
        ExpressionFactory::makeKeyword(":has", false);
}

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
        ScamExpr * key = input[idx];
        ScamExpr * val = input[idx+1];
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

void ScamDict::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    if ( args->isNil() ) {
        stringstream s;
        s << "Dict expected ':op args...'; got " << args->toString();
        ScamExpr * err
            = ExpressionFactory::makeError(s.str());
        cont->run(err);
        return;
    }

    exec(args, cont);
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

bool ScamDict::equals(ScamExpr const * expr) const
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
        ScamExpr const * myKey = this->keys[thisIdx];
        for ( otherIdx = 0 ; otherIdx < len ; ++otherIdx ) {
            if ( that->keys[otherIdx]->equals(myKey) ) {
                ScamExpr const * myVal = this->vals[thisIdx];
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

bool ScamDict::has(ScamExpr const * key) const
{
    for ( size_t jdx = 0 ; jdx < keys.size() ; ++jdx ) {
        if ( keys[jdx]->equals(key) ) {
            return true;
        }
    }

    return false;
}

ScamExpr * ScamDict::get(ScamExpr const * key) const
{
    for ( size_t jdx = 0 ; jdx < keys.size() ; ++jdx ) {
        if ( keys[jdx]->equals(key) ) {
            return vals[jdx];
        }
    }

    stringstream s;
    s << "Dict key '" << key->toString() << "' does not exist";
    return ExpressionFactory::makeError(s.str());
}

ScamExpr * ScamDict::put(ScamExpr const * key, ScamExpr * val)
{
    size_t prev = keys.size();
    ScamExpr * v = val;

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

ScamExpr * ScamDict::remove(ScamExpr const * key)
{
    ScamExpr * rv = ExpressionFactory::makeNil();

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

void ScamDict::bad_op(ScamExpr * op, Continuation * cont)
{
    stringstream s;
    s << "Dict expects op = [:get|:put|:length|:remove|:has]; got "
      << op->toString();
    ScamExpr * msg = ExpressionFactory::makeError(s.str());
    cont->run(msg);
}

void ScamDict::exec_get(ScamExpr * args, Continuation * cont)
{
    if ( args->length() < 2u ) {
        ScamExpr * err
            = ExpressionFactory::makeError("dict :get missing key to get");
        cont->run(err);
    }
    else {
        ScamExpr * key = args->nthcar(1);
        ScamExpr * val = get(key);
        cont->run(val);
    }
}

void ScamDict::exec_put(ScamExpr * args, Continuation * cont)
{
    if ( args->length() < 3u ) {
        stringstream s;
        s << "dict :put expects key, value; got: " << args->toString();
        ScamExpr * err = ExpressionFactory::makeError(s.str());
        cont->run(err);
    }
    else {
        ScamExpr * key = args->nthcar(1);
        ScamExpr * val = args->nthcar(2);
        ScamExpr * rv  = put(key, val);
        cont->run(rv);
    }
}

void ScamDict::exec_has(ScamExpr * args, Continuation * cont)
{
    if ( args->length() < 2u ) {
        stringstream s;
        s << "dict :has expects key; got: " << args->toString();
        ScamExpr * err = ExpressionFactory::makeError(s.str());
        cont->run(err);
    }
    else {
        ScamExpr * key = args->nthcar(1);
        bool b = has(key);
        ScamExpr * rv = ExpressionFactory::makeBoolean(b);
        cont->run(rv);
    }
}

void ScamDict::exec_remove(ScamExpr * args, Continuation * cont)
{
    if ( args->length() < 2u ) {
        stringstream s;
        s << "dict :remove expects key; got: " << args->toString();
        ScamExpr * err = ExpressionFactory::makeError(s.str());
        cont->run(err);
    }
    else {
        ScamExpr * key = args->nthcar(1);
        ScamExpr * rv  = remove(key);
        cont->run(rv);
    }
}

void ScamDict::exec_length(Continuation * cont)
{
    ScamExpr * len = ExpressionFactory::makeInteger(length());
    cont->run(len);
}

void ScamDict::exec(ScamExpr * args, Continuation * cont)
{
    ScamExpr * op = args->nthcar(0);
    if ( ! op->isKeyword() ) {
        bad_op(op, cont);
        return;
    }

    if ( op->equals(getOp) ) {
        exec_get(args, cont);
    }
    else if ( op->equals(putOp) ) {
        exec_put(args, cont);
    }
    else if ( op->equals(lenOp) ) {
        exec_length(cont);
    }
    else if ( op->equals(hasOp) ) {
        exec_has(args, cont);
    }
    else if ( op->equals(remOp) ) {
        exec_remove(args, cont);
    }
    else {
        bad_op(op, cont);
    }
}
