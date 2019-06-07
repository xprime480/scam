#include "prim/StringOps.hpp"

#include "Continuation.hpp"
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
                              const char * name,
                              Transformer transform,
                              Comparator compare);

    extern void transformString(ScamValue args,
                                Continuation * cont,
                                const char * name,
                                Transformer transform);
}

void scam::applyString(ScamValue args,
                       Continuation * cont,
                       ScamEngine * engine)
{
    static const char * name { "string" };
    ArgListHelper helper(args);

    ScamValue chars = makeNil();
    if ( ! wantZeroPlus(name, helper, cont, chars, isChar) ) {
        return;
    }
    const char * msg { "only accepts one list of characters" };
    if ( ! finishArgs(name, helper, cont, msg) ) {
        return;
    }

    size_t count = length(chars);
    char * buffer = new char[1+count];
    buffer[count] = '\0';

    for ( size_t idx = 0 ; idx < count ; ++idx ) {
        buffer[idx] = asChar(nthcar(chars, idx));
    }

    string newText(buffer);
    cont->run(makeString(newText));

    delete[] buffer;
}

void scam::applyMakeString(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine)
{
    static const char * name { "make-string" };
    ArgListHelper helper(args);

    int count;
    if ( ! wantNonNegativeInteger(name, helper, cont, count) ) {
        return;
    }
    char c = wantOptional<char>(name, helper, cont, isChar, asChar, ' ');
    if ( ! finishArgs(name, helper, cont) ) {
        return;
    }

    string newText(count, c);
    ScamValue newValue = makeString(newText);
    cont->run(newValue);
}

void scam::applyStringLength(ScamValue args,
                             Continuation * cont,
                             ScamEngine * engine)
{
    static const char * name { "string-length" };
    ArgListHelper helper(args);

    string value;
    if ( ! wantString(name, helper, cont, value) ) {
        return;
    }
    if ( ! finishArgs(name, helper, cont) ) {
        return;
    }

    cont->run(makeInteger(value.size(), true));
}

void scam::applyStringRef(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    static const char * name { "string-ref" };
    ArgListHelper helper(args);

    string str;
    if ( ! wantString(name, helper, cont, str) ) {
        return;
    }
    int idx;
    if ( ! wantIndex(name, helper, cont, idx, 0) ) {
        return;
    }
    if ( ! finishArgs(name, helper, cont) ) {
        return;
    }

    const char c     = str.at(idx);
    cont->run(makeCharacter(c));
}

void scam::applyStringSetX(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine)
{
    static const char * name { "string-set!" };
    ArgListHelper helper(args);

    ScamValue original;
    if ( ! wantMutableString(name, helper, cont, original) ) {
        return;
    }
    int idx;
    if ( ! wantIndex(name, helper, cont, idx, 0) ) {
        return;
    }
    char c;
    if ( ! wantChar(name, helper, cont, c) ) {
        return;
    }
    if ( ! finishArgs(name, helper, cont) ) {
        return;
    }

    const string str = asString(original);

    const auto size = str.size();
    char * buffer = new char[1 + size];
    buffer[size] = '\0';
    memcpy(buffer, str.c_str(), str.size());
    buffer[idx] = c;

    STRVAL(original) = string(buffer);
    cont->run(original);

    delete[] buffer;
}

void scam::applyStringEqP(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    static const char * name { "string=?" };
    compareString(args, cont, name, identityString, eqString());
}

void scam::applyStringCiEqP(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string-ci=?" };
    compareString(args, cont, name, downcaseString, eqString());
}

void scam::applyStringLtP(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    static const char * name { "string<?" };
    compareString(args, cont, name, identityString, ltString());
}

void scam::applyStringCiLtP(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string-ci<?" };
    compareString(args, cont, name, downcaseString, ltString());
}

void scam::applyStringLeP(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    static const char * name { "string<=?" };
    compareString(args, cont, name, identityString, leString());
}

void scam::applyStringCiLeP(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string-ci<=?" };
    compareString(args, cont, name, downcaseString, leString());
}

void scam::applyStringGtP(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    static const char * name { "string>?" };
    compareString(args, cont, name, identityString, gtString());
}

void scam::applyStringCiGtP(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string-ci>?" };
    compareString(args, cont, name, downcaseString, gtString());
}

void scam::applyStringGeP(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    static const char * name { "string>=?" };
    compareString(args, cont, name, identityString, geString());
}

void scam::applyStringCiGeP(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string-ci>=?" };
    compareString(args, cont, name, downcaseString, geString());
}

void scam::applyStringUpcase(ScamValue args,
                             Continuation * cont,
                             ScamEngine * engine)
{
    transformString(args, cont, "string-upcase", upcaseString);
}

void scam::applyStringDowncase(ScamValue args,
                               Continuation * cont,
                               ScamEngine * engine)
{
    transformString(args, cont, "string-downcase", downcaseString);
}

void scam::applyStringAppend(ScamValue args,
                             Continuation * cont,
                             ScamEngine * engine)
{
    static const char * name { "string-append" };
    ArgListHelper helper(args);

    ScamValue strs = makeNil();
    if ( ! wantZeroPlus(name, helper, cont, strs, isString) ) {
        return;
    }
    const char * msg { "only accepts string parameters" };
    if ( ! finishArgs(name, helper, cont, msg) ) {
        return;
    }

    const int len = length(strs);
    stringstream s;
    for ( int idx = 0 ; idx < len ; ++idx ) {
        s << asString(nthcar(strs, idx));
    }

    cont->run(makeString(s.str()));
}

void scam::applyString2List(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string->list" };
    ArgListHelper helper(args);

    string str;
    if ( ! wantString(name, helper, cont, str) ) {
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
    const char * msg { "invalid parameter list" };
    if ( ! finishArgs(name, helper, cont, msg) ) {
        return;
    }

    string newStr = start == end ? string("") : str.substr(start, end-start);
    vector<ScamValue> chars;

    for ( const char c : newStr ) {
        chars.push_back(makeCharacter(c));
    }

    cont->run(makeList(chars));
}

extern void scam::applyList2String(ScamValue args,
                                   Continuation * cont,
                                   ScamEngine * engine)
{
    static const char * name { "list->string" };
    ArgListHelper helper(args);

    ScamValue chars = makeNil();
    if ( ! wantSublistOf(name, helper, cont, chars, isChar) ) {
        return;
    }
    const char * msg { "only accepts character parameters" };
    if ( ! finishArgs(name, helper, cont, msg) ) {
        return;
    }

    const int len = length(chars);

    stringstream s;
    for ( int idx = 0 ; idx < len ; ++idx ) {
        s << asChar(nthcar(chars, idx));
    }

    cont->run(makeString(s.str()));
}

void scam::applyStringCopy(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine)
{
    static const char * name { "string-copy" };
    ArgListHelper helper(args);

    string str;
    if ( ! wantString(name, helper, cont, str) ) {
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
    const char * msg { "invalid argument list" };
    if ( ! finishArgs(name, helper, cont, msg) ) {
        return;
    }

    string newStr = start == end ? string("") : str.substr(start, end-start);

    cont->run(makeString(newStr));
}

void scam::applyStringCopyX(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string-copy!" };
    ArgListHelper helper(args);

    ScamValue toValue;
    if ( ! wantMutableString(name, helper, cont, toValue) ) {
        return;
    }
    int at;
    if ( ! wantIndex(name, helper, cont, at, 0) ) {
        return;
    }
    string fromStr;
    if ( ! wantString(name, helper, cont, fromStr) ) {
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
    const char * msg { "invalid argument list" };
    if ( ! finishArgs(name, helper, cont, msg) ) {
        return;
    }

    string toStr = asString(toValue);

    const unsigned int count = end - start;
    if ( 0 == count ) {
        cont->run(toValue);
        return;
    }

    const unsigned int space = toStr.size() - at;
    if ( space < count ) {
        ScamValue err =
            makeErrorExtended(name,
                              ": Insufficient room in destination",
                              " to copy source");
        cont->run(err);
        return;
    }

    char * buffer = new char[1 + toStr.size()];
    memcpy(buffer, toStr.c_str(), toStr.size());
    buffer[toStr.size()] = '\0';

    for ( unsigned int idx = 0 ; idx < count ; ++idx ) {
        buffer[idx + at] = fromStr.at(idx + start);
    }

    STRVAL(toValue) = string(buffer);
    delete[] buffer;

    cont->run(toValue);
}

void scam::applyStringFillX(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string-fill!" };
    ArgListHelper helper(args);

    ScamValue strValue;
    if ( ! wantMutableString(name, helper, cont, strValue) ) {
        return;
    }
    string toStr = asString(strValue);

    char fill;
    if ( ! wantChar(name, helper, cont, fill) ) {
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
    const char * msg { "invalid parameter list" };
    if ( ! finishArgs(name, helper, cont, msg) ) {
        return;
    }

    if ( start == end ) {
        cont->run(strValue);
        return;
    }

    char * buffer = new char[1 + toStr.size()];
    memcpy(buffer, toStr.c_str(), toStr.size());
    buffer[toStr.size()] = '\0';

    for ( int idx = start ; idx < end ; ++idx ) {
        buffer[idx] = fill;
    }

    STRVAL(strValue) = string(buffer);
    delete[] buffer;

    cont->run(strValue);
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
                       const char * name,
                       Transformer transform,
                       Comparator compare)
    {
        ArgListHelper helper(args);

        ScamValue strs;
        if ( ! wantCount(name, helper, cont, strs, isString, 2, 9999) ) {
            return;
        }
        const char * msg { "invalid parameter list" };
        if ( ! finishArgs(name, helper, cont, msg) ) {
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

        cont->run(makeBoolean(rv));
    }

    void transformString(ScamValue args,
                         Continuation * cont,
                         const char * name,
                         Transformer transformer)
    {
        ArgListHelper helper(args);

        string text;
        if ( ! wantString(name, helper, cont, text) ) {
            return;
        }
        const char * msg { "invalid parameter list" };
        if ( ! finishArgs(name, helper, cont, msg) ) {
            return;
        }

        string newText = transformer(text);
        cont->run(makeString(newText));
    }
}
