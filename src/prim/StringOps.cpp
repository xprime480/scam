#include "prim/StringOps.hpp"

#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "expr/ScamData.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "input/ListParser.hpp"
#include "util/ArgListHelper.hpp"

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
    static const char * name { "string" };
    ArgListHelper helper(args);

    ScamValue chars = makeNull();
    if ( ! wantZeroPlus(name, helper, cont, engine, chars, isChar) ) {
        return;
    }
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    size_t count = length(chars);
    char * buffer = new char[1+count];
    buffer[count] = '\0';

    for ( size_t idx = 0 ; idx < count ; ++idx ) {
        buffer[idx] = asChar(nthcar(chars, idx));
    }

    string newText(buffer);
    cont->handleValue(makeString(newText));

    delete[] buffer;
}

void scam::applyMakeString(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine)
{
    static const char * name { "make-string" };
    ArgListHelper helper(args);

    int count;
    if ( ! wantNonNegativeInteger(name, helper, cont, engine, count) ) {
        return;
    }
    char c = wantOptional<char>(name, helper, cont, isChar, asChar, ' ');
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    string newText(count, c);
    ScamValue newValue = makeString(newText);
    cont->handleValue(newValue);
}

void scam::applyStringLength(ScamValue args,
                             Continuation * cont,
                             ScamEngine * engine)
{
    static const char * name { "string-length" };
    ArgListHelper helper(args);

    string value;
    if ( ! wantString(name, helper, cont, engine, value) ) {
        return;
    }
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    cont->handleValue(makeInteger(value.size(), true));
}

void scam::applyStringRef(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    static const char * name { "string-ref" };
    ArgListHelper helper(args);

    string str;
    if ( ! wantString(name, helper, cont, engine, str) ) {
        return;
    }
    int idx;
    if ( ! wantIndex(name, helper, cont, engine, idx, 0) ) {
        return;
    }
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    const char c     = str.at(idx);
    cont->handleValue(makeCharacter(c));
}

void scam::applyStringSetX(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine)
{
    static const char * name { "string-set!" };
    ArgListHelper helper(args);

    ScamValue original;
    if ( ! wantMutableString(name, helper, cont, engine, original) ) {
        return;
    }
    int idx;
    if ( ! wantIndex(name, helper, cont, engine, idx, 0) ) {
        return;
    }
    char c;
    if ( ! wantChar(name, helper, cont, engine, c) ) {
        return;
    }
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    const string str = asString(original);

    const auto size = str.size();
    char * buffer = new char[1 + size];
    buffer[size] = '\0';
    memcpy(buffer, str.c_str(), str.size());
    buffer[idx] = c;

    original->stringValue() = string(buffer);
    cont->handleValue(original);

    delete[] buffer;
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
    static const char * name { "string-append" };
    ArgListHelper helper(args);

    ScamValue strs = makeNull();
    if ( ! wantZeroPlus(name, helper, cont, engine, strs, isString) ) {
        return;
    }
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    const int len = length(strs);
    stringstream s;
    for ( int idx = 0 ; idx < len ; ++idx ) {
        s << asString(nthcar(strs, idx));
    }

    cont->handleValue(makeString(s.str()));
}

void scam::applyString2List(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string->list" };
    ArgListHelper helper(args);

    string str;
    if ( ! wantString(name, helper, cont, engine, str) ) {
        return;
    }
    int start = wantOptional<int>(name,
                                  helper,
                                  cont,
                                  isStartIndexOf<string>(str),
                                  asInteger,
                                  0);
    int end = wantOptional<int>(name,
                                helper,
                                cont,
                                isEndIndexOf<string>(str, start),
                                asInteger,
                                str.size());
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    string newStr = start == end ? string("") : str.substr(start, end-start);
    vector<ScamValue> chars;

    for ( const char c : newStr ) {
        chars.push_back(makeCharacter(c));
    }

    cont->handleValue(makeList(chars));
}

extern void scam::applyList2String(ScamValue args,
                                   Continuation * cont,
                                   ScamEngine * engine)
{
    static const char * name { "list->string" };
    ArgListHelper helper(args);

    ScamValue chars = makeNull();
    if ( ! wantSublistOf(name, helper, cont, engine, chars, isChar) ) {
        return;
    }
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    const int len = length(chars);

    stringstream s;
    for ( int idx = 0 ; idx < len ; ++idx ) {
        s << asChar(nthcar(chars, idx));
    }

    cont->handleValue(makeString(s.str()));
}

void scam::applyStringCopy(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine)
{
    static const char * name { "string-copy" };
    ArgListHelper helper(args);

    string str;
    if ( ! wantString(name, helper, cont, engine, str) ) {
        return;
    }
    int start = wantOptional<int>(name,
                                  helper,
                                  cont,
                                  isStartIndexOf<string>(str),
                                  asInteger,
                                  0);
    int end = wantOptional<int>(name,
                                helper,
                                cont,
                                isEndIndexOf<string>(str, start),
                                asInteger,
                                str.size());
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    string newStr = start == end ? string("") : str.substr(start, end-start);

    cont->handleValue(makeString(newStr));
}

void scam::applyStringCopyX(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string-copy!" };
    ArgListHelper helper(args);

    ScamValue toValue;
    if ( ! wantMutableString(name, helper, cont, engine, toValue) ) {
        return;
    }
    int at;
    if ( ! wantIndex(name, helper, cont, engine, at, 0) ) {
        return;
    }
    string fromStr;
    if ( ! wantString(name, helper, cont, engine, fromStr) ) {
        return;
    }
    int start = wantOptional<int>(name,
                                  helper,
                                  cont,
                                  isStartIndexOf<string>(fromStr),
                                  asInteger,
                                  0);
    int end = wantOptional<int>(name,
                                helper,
                                cont,
                                isEndIndexOf<string>(fromStr, start),
                                asInteger,
                                fromStr.size());
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    string toStr = asString(toValue);

    const unsigned int count = end - start;
    if ( 0 == count ) {
        cont->handleValue(toValue);
        return;
    }

    const unsigned int space = toStr.size() - at;
    if ( space < count ) {
        ScamValue err =
            makeError("string-copy!: "
                      "Insufficient room in destination to copy source");
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

void scam::applyStringFillX(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string-fill!" };
    ArgListHelper helper(args);

    ScamValue strValue;
    if ( ! wantMutableString(name, helper, cont, engine, strValue) ) {
        return;
    }
    string toStr = asString(strValue);

    char fill;
    if ( ! wantChar(name, helper, cont, engine, fill) ) {
        return;
    }
    int start = wantOptional<int>(name,
                                  helper,
                                  cont,
                                  isStartIndexOf<string>(toStr),
                                  asInteger,
                                  0);
    int end = wantOptional<int>(name,
                                helper,
                                cont,
                                isEndIndexOf<string>(toStr, start),
                                asInteger,
                                toStr.size());
    if ( ! finishArgs(name, helper, cont, engine) ) {
        return;
    }

    if ( start == end ) {
        cont->handleValue(strValue);
        return;
    }

    char * buffer = new char[1 + toStr.size()];
    memcpy(buffer, toStr.c_str(), toStr.size());
    buffer[toStr.size()] = '\0';

    for ( int idx = start ; idx < end ; ++idx ) {
        buffer[idx] = fill;
    }

    strValue->stringValue() = string(buffer);
    delete[] buffer;

    cont->handleValue(strValue);
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
        ArgListHelper helper(args);

        ScamValue strs;
        bool ok =
            wantCount(name, helper, cont, engine, strs, isString, 2, 9999);
        if ( ! ok ) {
            return;
        }
        if ( ! finishArgs(name, helper, cont, engine) ) {
            return;
        }

        int len = length(strs);
        bool rv = true;

        string prev = transform(asString(nthcar(strs, 0)));
        for ( int idx = 1 ; (idx < len) && rv ; ++idx ) {
            const string curr = transform(asString(nthcar(strs, idx)));
            rv = compare(prev, curr);
            prev = curr;
        }

        cont->handleValue(makeBoolean(rv));
    }

    void transformString(ScamValue args,
                         Continuation * cont,
                         ScamEngine * engine,
                         const char * name,
                         Transformer transformer)
    {
        ArgListHelper helper(args);

        string text;
        if ( ! wantString(name, helper, cont, engine, text) ) {
            return;
        }
        if ( ! finishArgs(name, helper, cont, engine) ) {
            return;
        }

        string newText = transformer(text);
        cont->handleValue(makeString(newText));
    }
}
