#include "util/LambdaDef.hpp"

#include "ErrorCategory.hpp"
#include "expr/SequenceOps.hpp"
#include "util/Parameter.hpp"
#include "value/ScamData.hpp"
#include "value/TypePredicates.hpp"
#include "value/ValueFactory.hpp"

#include <set>

using namespace scam;
using namespace std;

LambdaDef::LambdaDef()
{
    reset();
}

void LambdaDef::mark() const
{
    formals->mark();
    rest->mark();
    forms->mark();
}

ScamValue LambdaDef::transform(ScamValue args)
{
    reset();

    ScamValue rv = getFormals(args);
    if ( ! isUnhandledError(rv) ) {
        rv = getForms(rv);
        if ( ! isUnhandledError(rv) ) {
            valid = true;
        }
    }

    return rv;
}

void LambdaDef::reset()
{
    valid = false;
    formals = makeNull();
    rest = makeNothing();
    forms = makeNull();
}

ScamValue LambdaDef::getFormals(ScamValue args)
{
    ObjectParameter p0;
    ScamValue rv = p0.transform(args);

    if ( ! isUnhandledError(rv) ) {
        ScamValue temp = p0.value;
        if ( isNull(temp) ) {
            // nothing, formals are as the default
        }
        else if ( isSymbol(temp) ) {
            rest = temp;
        }
        else if ( isPair(temp) ) {
            ScamValue tempRv = transformPair(temp);
            if ( isUnhandledError(tempRv) ) {
                rv = tempRv;
            }
        }
        else {
            rv = badFormals(temp);
        }
    }

    return rv;
}

ScamValue LambdaDef::transformPair(ScamValue args)
{
    set<string> formalNames;
    vector<ScamValue> temp;
    ScamValue original = args;
    ScamValue rv = makeNothing();

    while ( isPair(args) ) {
        ScamValue head = getCar(args);
        ScamValue tail = getCdr(args);

        if ( isSymbol(head) ) {
            if ( formalNames.end() !=
                 formalNames.find(head->stringValue()) ) {
                rv = duplicateFormal(head);
                break;
            }
            formalNames.insert(head->stringValue());
            temp.push_back(head);
            args = tail;
        }
        else {
            rv = badFormals(original);
            break;
        }
    }

    if ( ! isUnhandledError(rv) ) {
        if ( isSymbol(args) ) {
            if ( formalNames.end() !=
                 formalNames.find(args->stringValue()) ) {
                rv = duplicateFormal(original);
            }
            rest = args;
        }
        else if ( ! isNull(args) ) {
            rv = badFormals(original);
        }
    }

    if ( ! isUnhandledError(rv) ) {
        formals = makeList(temp);
    }

    return rv;
}

ScamValue LambdaDef::getForms(ScamValue args)
{
    ObjectParameter  pObj;
    CountedParameter p0(pObj);
    ScamValue rv = p0.transform(args);
    if ( ! isUnhandledError(rv) ) {
        forms = p0.value;
    }
    return rv;
}

ScamValue LambdaDef::badFormals(ScamValue value)
{
    static const char * text = "Expecting formals, got %{0}";
    ScamValue err = makeError(text, value);
    err->errorCategory() = argsCategory;
    return err;
}

ScamValue LambdaDef::duplicateFormal(ScamValue value)
{
    static const char * text = "Duplicate in formals list: %{0}";
    ScamValue err = makeError(text, value);
    err->errorCategory() = argsCategory;
    return err;
}
