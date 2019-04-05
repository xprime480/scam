#include "expr/ScamDict.hpp"

#include "Continuation.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void
    do_apply(ScamExpr * args, ContHandle cont, Env env, ScamDict * dict);
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

void ScamDict::apply(ScamExpr * args, ContHandle cont, Env env)
{
    do_apply(args, cont, env, this);
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

namespace
{
    static const ScamExpr * getOp = ExpressionFactory::makeKeyword(":get");
    static const ScamExpr * putOp = ExpressionFactory::makeKeyword(":put");
    static const ScamExpr * lenOp = ExpressionFactory::makeKeyword(":length");
    static const ScamExpr * remOp = ExpressionFactory::makeKeyword(":remove");
    static const ScamExpr * hasOp = ExpressionFactory::makeKeyword(":has");

    void bad_op(ScamExpr * op, ContHandle cont)
    {
        stringstream s;
        s << "Dict expects op = [:get|:put|:length|:remove|:has]; got "
          << op->toString();
        ScamExpr * msg = ExpressionFactory::makeError(s.str());
        cont->run(msg);
    }

    void exec_get(ScamExpr * args, ContHandle cont, ScamDict * dict)
    {
        if ( args->length() < 2u ) {
            ScamExpr * err
                = ExpressionFactory::makeError("dict :get missing key to get");
            cont->run(err);
        }
        else {
            ScamExpr * key = args->nthcar(1);
            ScamExpr * val = dict->get(key);
            cont->run(val);
        }
    }

    void exec_put(ScamExpr * args, ContHandle cont, ScamDict * dict)
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
            ScamExpr * rv = dict->put(key, val);

            cont->run(rv);
        }
    }

    void exec_has(ScamExpr * args, ContHandle cont, ScamDict * dict)
    {
        if ( args->length() < 2u ) {
            stringstream s;
            s << "dict :has expects key; got: " << args->toString();
            ScamExpr * err = ExpressionFactory::makeError(s.str());
            cont->run(err);
        }
        else {
            ScamExpr * key = args->nthcar(1);
            bool b = dict->has(key);
            ScamExpr * rv = ExpressionFactory::makeBoolean(b);

            cont->run(rv);
        }
    }

    void exec_remove(ScamExpr * args, ContHandle cont, ScamDict * dict)
    {
        if ( args->length() < 2u ) {
            stringstream s;
            s << "dict :remove expects key; got: " << args->toString();
            ScamExpr * err = ExpressionFactory::makeError(s.str());
            cont->run(err);
        }
        else {
            ScamExpr * key = args->nthcar(1);
            ScamExpr * rv = dict->remove(key);

            cont->run(rv);
        }
    }

    void exec_length(ContHandle cont, ScamDict * dict)
    {
        ScamExpr * len = ExpressionFactory::makeInteger(dict->length());
        cont->run(len);
    }

    void exec(ScamExpr * args, ContHandle cont, ScamDict * dict)
    {
        ScamExpr * op = args->nthcar(0);
        if ( ! op->isKeyword() ) {
            bad_op(op, cont);
            return;
        }

        if ( op->equals(getOp) ) {
            exec_get(args, cont, dict);
        }
        else if ( op->equals(putOp) ) {
            exec_put(args, cont, dict);
        }
        else if ( op->equals(lenOp) ) {
            exec_length(cont, dict);
        }
        else if ( op->equals(hasOp) ) {
            exec_has(args, cont, dict);
        }
        else if ( op->equals(remOp) ) {
            exec_remove(args, cont, dict);
        }
        else {
            bad_op(op, cont);
        }
    }

    void do_apply(ScamExpr * args, ContHandle cont, Env env, ScamDict * dict)
    {
        if ( args->isNil() ) {
            stringstream s;
            s << "Dict expected ':op args...'; got " << args->toString();
            ScamExpr * err
                = ExpressionFactory::makeError(s.str());
            cont->run(err);
            return;
        }

        exec(args, cont, dict);
    }
}
