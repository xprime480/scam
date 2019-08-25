#include "prim/StringOps.hpp"

#include "Continuation.hpp"
#include "ErrorCategory.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "util/Parameter.hpp"

#include <algorithm>
#include <cctype>
#include <cstring>

using namespace scam;
using namespace std;

namespace
{
    using eqString = equal_to<string>;
    using ltString = less<string>;
    using leString = less_equal<string>;
    using gtString = greater<string>;
    using geString = greater_equal<string>;

    using Transformer = function<string(const string &)>;

    extern string upcaseString(const string & str);
    extern string downcaseString(const string & str);
    extern string identityString(const string & str);

    extern char upcaseChar(char c);
    extern char downcaseChar(char c);

    template <typename Comparator>
    extern void compareString(ScamValue args,
                              Continuation * cont,
                              ScamEngine * engine,
                              const char * name,
                              Transformer transform,
                              Comparator compare);

    extern void transformString(ScamValue args,
                                Continuation * cont,
                                ScamEngine * engine,
                                const char * name,
                                Transformer transform);

}

void scam::applyString(ScamValue args,
                       Continuation * cont,
                       ScamEngine * engine)
{
    CharacterParameter pChar;
    CountedParameter p0(pChar);
    if ( argsToParms(args, engine, "string", p0) ) {
        ScamValue chars = p0.value;
        size_t count = length(chars);
        char * buffer = new char[1+count];
        buffer[count] = '\0';

        for ( size_t idx = 0 ; idx < count ; ++idx ) {
            buffer[idx] = asChar(nthcar(chars, idx));
        }

        string newText(buffer);
        ScamValue rv = makeString(newText);
        delete[] buffer;

        cont->handleValue(rv);
    }
}

void scam::applyMakeString(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine)
{
    CountParameter p0;
    CharacterParameter pChar;
    OptionalParameter p1(pChar);
    if ( argsToParms(args, engine, "make-string", p0, p1) ) {
        int count = asInteger(p0.value);
        char c = ' ';
        if ( ! isNothing(p1.value) ) {
            c = asChar(p1.value);
        }
        string newText(count, c);
        cont->handleValue(makeString(newText));
    }
}

void scam::applyStringLength(ScamValue args,
                             Continuation * cont,
                             ScamEngine * engine)
{
    StringParameter p0;
    if ( argsToParms(args, engine, "string-length", p0) ) {
        const string s = asString(p0.value);
        cont->handleValue(makeInteger(s.size(), true));
    }
}

void scam::applyStringRef(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    StringParameter p0;
    IndexParameter p1(p0);
    if ( argsToParms(args, engine, "string-ref", p0, p1) ) {
        const string str = asString(p0.value);
        const int idx    = asInteger(p1.value);
        const char c     = str.at(idx);
        cont->handleValue(makeCharacter(c));
    }
}

void scam::applyStringSetX(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine)
{
    StringParameter    pStr;
    MutableParameter   p0(pStr);
    IndexParameter     p1(p0);
    CharacterParameter p2;

    if ( argsToParms(args, engine, "string-set!", p0, p1, p2) ) {
        ScamValue original  = p0.value;
        const string str    = asString(original);
        const unsigned size = str.size();
        const int idx       = asInteger(p1.value);
        const char c        = asChar(p2.value);

        char * buffer = new char[1 + size];
        buffer[size] = '\0';
        memcpy(buffer, str.c_str(), size);
        buffer[idx] = c;

        original->stringValue() = string(buffer);
        delete[] buffer;
        cont->handleValue(original);
    }
}

void scam::applyStringEqP(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    static const char * name { "string=?" };
    compareString(args, cont, engine, name, identityString, eqString());
}

void scam::applyStringCiEqP(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string-ci=?" };
    compareString(args, cont, engine, name, downcaseString, eqString());
}

void scam::applyStringLtP(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    static const char * name { "string<?" };
    compareString(args, cont, engine, name, identityString, ltString());
}

void scam::applyStringCiLtP(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string-ci<?" };
    compareString(args, cont, engine, name, downcaseString, ltString());
}

void scam::applyStringLeP(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    static const char * name { "string<=?" };
    compareString(args, cont, engine, name, identityString, leString());
}

void scam::applyStringCiLeP(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string-ci<=?" };
    compareString(args, cont, engine, name, downcaseString, leString());
}

void scam::applyStringGtP(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    static const char * name { "string>?" };
    compareString(args, cont, engine, name, identityString, gtString());
}

void scam::applyStringCiGtP(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string-ci>?" };
    compareString(args, cont, engine, name, downcaseString, gtString());
}

void scam::applyStringGeP(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    static const char * name { "string>=?" };
    compareString(args, cont, engine, name, identityString, geString());
}

void scam::applyStringCiGeP(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string-ci>=?" };
    compareString(args, cont, engine, name, downcaseString, geString());
}

void scam::applyStringUpcase(ScamValue args,
                             Continuation * cont,
                             ScamEngine * engine)
{
    transformString(args, cont, engine, "string-upcase", upcaseString);
}

void scam::applyStringDowncase(ScamValue args,
                               Continuation * cont,
                               ScamEngine * engine)
{
    transformString(args, cont, engine, "string-downcase", downcaseString);
}

void scam::applyStringAppend(ScamValue args,
                             Continuation * cont,
                             ScamEngine * engine)
{
    StringParameter  pStr;
    CountedParameter p0(pStr);
    if ( argsToParms(args, engine, "string-append", p0) ) {
        ScamValue strs = p0.value ;
        const int len = length(strs);
        stringstream s;
        for ( int idx = 0 ; idx < len ; ++idx ) {
            s << asString(nthcar(strs, idx));
        }

        cont->handleValue(makeString(s.str()));
    }
}

void scam::applyString2List(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    StringParameter p0;
    StartIndexParameter pStart(p0);
    OptionalParameter p1(pStart);
    EndIndexParameter pEnd(p0, pStart);
    OptionalParameter p2(pEnd);

    if ( argsToParms(args, engine, "eq?", p0, p1, p2) ) {
        string str = asString(p0.value);
        int start  = 0;
        int end    = str.size();

        if ( ! isNothing(p1.value) ) {
            start = asInteger(p1.value);
        }
        if ( ! isNothing(p2.value) ) {
            end = asInteger(p2.value);
        }

        string newStr =
            start == end ? string("") : str.substr(start, end-start);
        vector<ScamValue> chars;
        for ( const char c : newStr ) {
            chars.push_back(makeCharacter(c));
        }

        cont->handleValue(makeList(chars));
    }
}

extern void scam::applyList2String(ScamValue args,
                                   Continuation * cont,
                                   ScamEngine * engine)
{
    CharacterParameter pChar;
    ListOfParameter p0(pChar);
    if ( argsToParms(args, engine, "list->string", p0) ) {
        ScamValue chars = p0.value;
        const int len = length(chars);

        stringstream s;
        for ( int idx = 0 ; idx < len ; ++idx ) {
            s << asChar(nthcar(chars, idx));
        }

        cont->handleValue(makeString(s.str()));
    }
}

void scam::applyStringCopy(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine)
{
    StringParameter p0;
    StartIndexParameter pStart(p0);
    OptionalParameter p1(pStart);
    EndIndexParameter pEnd(p0, pStart);
    OptionalParameter p2(pEnd);

    if ( argsToParms(args, engine, "string-copy", p0, p1, p2) ) {
        string str = asString(p0.value);
        int start = 0;
        int end = str.size();

        if ( ! isNothing(p1.value) ) {
            start = asInteger(p1.value);
        }
        if ( ! isNothing(p2.value) ) {
            end = asInteger(p2.value);
        }

        string newStr;
        if ( start < end ) {
            newStr = str.substr(start, end-start);
        }

        cont->handleValue(makeString(newStr));
    }
}

void scam::applyStringCopyX(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    StringParameter pStr;
    MutableParameter p0(pStr);
    StartIndexParameter p1(pStr);
    StringParameter p2;
    StartIndexParameter pStart(p2);
    OptionalParameter p3(pStart);
    EndIndexParameter pEnd(p2, pStart);
    OptionalParameter p4(pEnd);

    if ( argsToParms(args, engine, "string-copy!", p0, p1, p2, p3, p4) ) {
        ScamValue toValue = p0.value;
        string toStr = asString(toValue);
        int at = asInteger(p1.value);
        string fromStr = asString(p2.value);
        int start = 0;
        int end   = fromStr.size();

        if ( ! isNothing(p3.value) ) {
            start = asInteger(p3.value);
        }
        if ( ! isNothing(p4.value) ) {
            end = asInteger(p4.value);
        }

        const unsigned int count = end - start;
        if ( 0 == count ) {
            cont->handleValue(toValue);
            return;
        }

        const unsigned int space = toStr.size() - at;
        if ( space < count ) {
            ScamValue err =
                makeError("Insufficient room in destination to copy source");
            err->errorCategory() = evalCategory;
            engine->handleError(err);
            return;
        }

        char * buffer = new char[1 + toStr.size()];
        memcpy(buffer, toStr.c_str(), toStr.size());
        buffer[toStr.size()] = '\0';

        for ( unsigned int idx = 0 ; idx < count ; ++idx ) {
            buffer[idx + at] = fromStr.at(idx + start);
        }

        toValue->stringValue() = string(buffer);
        delete[] buffer;

        cont->handleValue(toValue);
    }
}

void scam::applyStringFillX(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    StringParameter pStr;
    MutableParameter p0(pStr);
    CharacterParameter p1;
    StartIndexParameter pStart(pStr);
    OptionalParameter p2(pStart);
    EndIndexParameter pEnd(pStr, pStart);
    OptionalParameter p3(pEnd);

    if ( argsToParms(args, engine, "eq?", p0, p1, p2, p3) ) {
        ScamValue toValue = p0.value;
        string toStr = asString(toValue);
        char fill = asChar(p1.value);
        int start = 0;
        int end = toStr.size();

        if ( ! isNothing(p2.value) ) {
            start = asInteger(p2.value);
        }
        if ( ! isNothing(p3.value) ) {
            end = asInteger(p3.value);
        }

        if ( start == end ) {
            cont->handleValue(toValue);
            return;
        }

        char * buffer = new char[1 + toStr.size()];
        memcpy(buffer, toStr.c_str(), toStr.size());
        buffer[toStr.size()] = '\0';

        for ( int idx = start ; idx < end ; ++idx ) {
            buffer[idx] = fill;
        }

        toValue->stringValue() = string(buffer);
        delete[] buffer;

        cont->handleValue(toValue);
    }
}

namespace
{
    string upcaseString(const string & str)
    {
        string newStr = str;
        transform(str.begin(), str.end(), newStr.begin(), upcaseChar);
        return newStr;
    }

    string downcaseString(const string & str)
    {
        string newStr = str;
        transform(str.begin(), str.end(), newStr.begin(), downcaseChar);
        return newStr;
    }

    string identityString(const string & str)
    {
        return str;
    }

    char upcaseChar(char c) { return toupper(c); }
    char downcaseChar(char c) { return tolower(c); }

    template <typename Comparator>
    void compareString(ScamValue args,
                       Continuation * cont,
                       ScamEngine * engine,
                       const char * name,
                       Transformer transform,
                       Comparator compare)
    {
        StringParameter pStr;
        CountedParameter p0(pStr, 2);
        if ( argsToParms(args, engine, name, p0) ) {
            ScamValue strs = p0.value;
            int len = length(strs);
            bool rv = true;

            string prev = transform(asString(nthcar(strs, 0)));
            for ( int idx = 1 ; (idx < len) && rv ; ++idx ) {
                const string curr = transform(asString(nthcar(strs, idx)));
                rv = compare(prev, curr);
                if ( ! rv ) {
                    break;
                }
                prev = curr;
            }

            cont->handleValue(makeBoolean(rv));
        }
    }

    void transformString(ScamValue args,
                         Continuation * cont,
                         ScamEngine * engine,
                         const char * name,
                         Transformer transformer)
    {
        StringParameter p0;
        if ( argsToParms(args, engine, name, p0) ) {
            string text    = asString(p0.value);
            string newText = transformer(text);
            cont->handleValue(makeString(newText));
        }
    }
}
