#include "util/LetDef.hpp"

#include "expr/ScamData.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"

using namespace scam;
using namespace std;

namespace
{
    struct BindingsParameter : public ObjectParameter
    {
        BindingsParameter(vector<ScamValue> & syms, vector<ScamValue> & vals);
        ScamValue transform(ScamValue args);

    private:
        vector<ScamValue> & syms;
        vector<ScamValue> & vals;
    };
}

LetDef::LetDef()
{
    reset();
}
void LetDef::mark()
{
    formals->mark();
    values->mark();
    forms->mark();
}

ScamValue LetDef::transform(ScamValue args)
{
    reset();

    ScamValue rv = getBindings(args);
    if ( ! isUnhandledError(rv) ) {
        rv = getForms(rv);
        if ( ! isUnhandledError(rv) ) {
            valid = true;
        }
    }

    return rv;
}

void LetDef::reset()
{
    formals = values = forms = makeNull();
}

ScamValue LetDef::getBindings(ScamValue args)
{
    vector<ScamValue> syms;
    vector<ScamValue> vals;
    BindingsParameter pBind(syms, vals);
    ListOfParameter   p0(pBind);

    ScamValue rv = p0.transform(args);
    if ( ! isUnhandledError(rv) )  {
        formals = makeList(syms);
        values  = makeList(vals);
    }

    return rv;
}

ScamValue LetDef::getForms(ScamValue args)
{
    ObjectParameter  pObj;
    CountedParameter p0(pObj);

    ScamValue rv = p0.transform(args);
    if ( ! isUnhandledError(rv) ) {
        forms = p0.value;
    }

    return rv;
}

namespace
{
    BindingsParameter::BindingsParameter(vector<ScamValue> & syms,
                                         vector<ScamValue> & vals)
        : syms(syms)
        , vals(vals)
    {
    }

    ScamValue BindingsParameter::transform(ScamValue args)
    {
        valid = false;
        ListParameter pList;

        ScamValue rv = pList.transform(args);

        if ( ! isUnhandledError(rv) ) {
            SymbolParameter p0;
            ScamValue temp = p0.transform(pList.value);
            if ( isUnhandledError(temp) ) {
                rv = temp;
            }
            else {
                ObjectParameter p1;
                temp = p1.transform(temp);
                if ( isUnhandledError(temp) ) {
                    rv = temp;
                }
                else {
                    valid = true;
                    syms.push_back(p0.value);
                    vals.push_back(p1.value);
                    value = makeList(p0.value, p1.value);
                }
            }
        }

        return rv;
    }
}
