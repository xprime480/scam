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

ScamDict::ScamDict(ExprVec const & args)
{
    ExprVec input = args;
    if ( 1 == (input.size() % 2) ) {
        input.push_back(ExpressionFactory::makeNil());
    }

    for ( size_t idx = 0 ; idx < input.size() ; idx += 2 ) {
        ExprHandle key = input[idx];
        ExprHandle val = input[idx+1];
        put(key.get(), val.get());
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
        ScamExpr const * myKey = this->keys[thisIdx].get();
        for ( otherIdx = 0 ; otherIdx < len ; ++otherIdx ) {
            if ( that->keys[otherIdx]->equals(myKey) ) {
                ScamExpr const * myVal = this->vals[thisIdx].get();
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

ExprHandle ScamDict::get(ScamExpr const * key) const
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

ExprHandle ScamDict::put(ScamExpr const * key, ScamExpr const * val)
{
    size_t prev = keys.size();
    ExprHandle v = val->clone();

    for ( size_t jdx = 0 ; jdx < keys.size() ; ++jdx ) {
        if ( keys[jdx]->equals(key) ) {
            prev = jdx;
            break;
        }
    }

    if ( prev >= keys.size() ) {
        keys.push_back(key->clone());
        vals.push_back(v);
    }
    else {
        vals[prev] = v;
    }

    return v;
}

ExprHandle ScamDict::remove(ScamExpr const * key)
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

ExprVec const & ScamDict::getKeys() const
{
    return keys;
}

namespace
{
    static const ExprHandle getOp = ExpressionFactory::makeKeyword(":get");
    static const ExprHandle putOp = ExpressionFactory::makeKeyword(":put");
    static const ExprHandle lenOp = ExpressionFactory::makeKeyword(":length");
    static const ExprHandle remOp = ExpressionFactory::makeKeyword(":remove");
    static const ExprHandle hasOp = ExpressionFactory::makeKeyword(":has");

    void bad_op(ScamExpr * op, ContHandle cont)
    {
        stringstream s;
        s << "Dict expects op = [:get|:put|:length|:remove|:has]; got "
          << op->toString();
        ExprHandle msg = ExpressionFactory::makeError(s.str());
        cont->run(msg.get());
    }

    void exec_get(ScamExpr * args, ContHandle cont, ScamDict * dict)
    {
        if ( args->length() < 2u ) {
            ExprHandle err
                = ExpressionFactory::makeError("dict :get missing key to get");
            cont->run(err.get());
        }
        else {
            ExprHandle key = args->nthcar(1);
            ExprHandle val = dict->get(key.get());
            cont->run(val.get());
        }
    }

    void exec_put(ScamExpr * args, ContHandle cont, ScamDict * dict)
    {
        if ( args->length() < 3u ) {
            stringstream s;
            s << "dict :put expects key, value; got: " << args->toString();
            ExprHandle err = ExpressionFactory::makeError(s.str());
            cont->run(err.get());
        }
        else {
            ExprHandle key = args->nthcar(1);
            ExprHandle val = args->nthcar(2);
            ExprHandle rv = dict->put(key.get(), val.get());

            cont->run(rv.get());
        }
    }

    void exec_has(ScamExpr * args, ContHandle cont, ScamDict * dict)
    {
        if ( args->length() < 2u ) {
            stringstream s;
            s << "dict :has expects key; got: " << args->toString();
            ExprHandle err = ExpressionFactory::makeError(s.str());
            cont->run(err.get());
        }
        else {
            ExprHandle key = args->nthcar(1);
            bool b = dict->has(key.get());
            ExprHandle rv = ExpressionFactory::makeBoolean(b);

            cont->run(rv.get());
        }
    }

    void exec_remove(ScamExpr * args, ContHandle cont, ScamDict * dict)
    {
        if ( args->length() < 2u ) {
            stringstream s;
            s << "dict :remove expects key; got: " << args->toString();
            ExprHandle err = ExpressionFactory::makeError(s.str());
            cont->run(err.get());
        }
        else {
            ExprHandle key = args->nthcar(1);
            ExprHandle rv = dict->remove(key.get());

            cont->run(rv.get());
        }
    }

    void exec_length(ContHandle cont, ScamDict * dict)
    {
        ExprHandle len = ExpressionFactory::makeInteger(dict->length());
        cont->run(len.get());
    }

    void exec(ScamExpr * args, ContHandle cont, ScamDict * dict)
    {
        ExprHandle op = args->nthcar(0);
        if ( ! op->isKeyword() ) {
            bad_op(op.get(), cont);
            return;
        }

        if ( op->equals(getOp.get()) ) {
            exec_get(args, cont, dict);
        }
        else if ( op->equals(putOp.get()) ) {
            exec_put(args, cont, dict);
        }
        else if ( op->equals(lenOp.get()) ) {
            exec_length(cont, dict);
        }
        else if ( op->equals(hasOp.get()) ) {
            exec_has(args, cont, dict);
        }
        else if ( op->equals(remOp.get()) ) {
            exec_remove(args, cont, dict);
        }
        else {
            bad_op(op.get(), cont);
        }
    }

    void do_apply(ScamExpr * args, ContHandle cont, Env env, ScamDict * dict)
    {
        if ( args->isNil() ) {
            stringstream s;
            s << "Dict expected ':op args...'; got " << args->toString();
            ExprHandle err
                = ExpressionFactory::makeError(s.str());
            cont->run(err.get());
            return;
        }

        exec(args, cont, dict);
    }
}
