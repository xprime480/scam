#include "prim/StringOps.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueFactory.hpp"
#include "expr/ValueWriter.hpp"
#include "input/ListParser.hpp"
#include "util/Validator.hpp"

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

    extern int valueOrDefault(ScamValue src, int defaultValue);
}

void scam::applyString(ScamValue args,
                       Continuation * cont,
                       ScamEngine * engine)
{
    static const char * name { "string" };
    Matcher matcher = matchCount("chars", "char", matchCharacter("char"));

    Callback callback = [cont] (const ValidatorResult & result) -> void
    {
        ScamValue value = result.get("chars");
        size_t count = length(value);
        char * buffer = new char[1+count];
        buffer[count] = '\0';

        for ( size_t idx = 0 ; idx < count ; ++idx ) {
            buffer[idx] = asChar(nthcar(value, idx));
        }

        string newText(buffer);
        cont->run(makeString(newText));

        delete[] buffer;
    };

    validate(name, args, cont, callback, matcher);
}

void scam::applyMakeString(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine)
{
    static const char * name { "make-string" };
    Matcher matcher =
        matchAlternative(name,
                         matchSequence(name,
                                       matchNonNegativeInteger("k"),
                                       matchCharacter("char")),
                         matchNonNegativeInteger("k"));

    Callback callback = [cont] (const ValidatorResult & result) -> void
    {
        const int count = asInteger(result.get("k"));
        char c = ' ';
        ScamValue value = result.get("char");
        if ( ! isNull(value) ) {
            c = asChar(value);
        }
        string newText(count, c);
        ScamValue newValue = makeString(newText);
        cont->run(newValue);
    };

    validate(name, args, cont, callback, matcher);
}

void scam::applyStringLength(ScamValue args,
                             Continuation * cont,
                             ScamEngine * engine)
{
    static const char * name { "string-length" };
    Matcher matcher = matchString("string");

    Callback callback = [cont] (const ValidatorResult & result) -> void
    {
        ScamValue value = result.get("string");
        cont->run(makeInteger(length(value), true));
    };

    validate(name, args, cont, callback, matcher);
}

void scam::applyStringRef(ScamValue args,
                          Continuation * cont,
                          ScamEngine * engine)
{
    static const char * name { "string-ref" };
    Matcher matcher =
        matchSequence(name,
                      matchString("string"),
                      matchIndex("k", "string"));

    Callback callback = [cont] (const ValidatorResult & result) -> void
    {
        const string str = asString(result.get("string"));
        const int idx    = asInteger(result.get("k"));
        const char c     = str.at(idx);
        cont->run(makeCharacter(c));
    };

    validate(name, args, cont, callback, matcher);
}

void scam::applyStringSetX(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine)
{
    static const char * name { "string-set!" };
    Matcher matcher =
        matchSequence(name,
                      matchString("string"),
                      matchIndex("k", "string"),
                      matchCharacter("char"));

    Callback callback = [cont] (const ValidatorResult & result) -> void
    {
        ScamValue original = result.get("string");
        if ( isImmutable(original) ) {
            ScamValue err =
                makeErrorExtended(name,
                                  ": Cannot mutate constant string ",
                                  writeValue(original));
            cont->run(err);
            return;
        }

        const string str = asString(original);
        const int idx    = asInteger(result.get("k"));
        const char c     = asChar(result.get("char"));

        const auto size = str.size();
        char * buffer = new char[1 + size];
        buffer[size] = '\0';
        memcpy(buffer, str.c_str(), str.size());
        buffer[idx] = c;

        STRVAL(original) = string(buffer);
        cont->run(original);

        delete[] buffer;
    };

    validate(name, args, cont, callback, matcher);
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
    Matcher matcher = matchCount("strings", "string", matchString("string"));

    Callback callback = [cont] (const ValidatorResult & result) -> void
    {
        ScamValue strs = result.get("strings");
        const int len = length(strs);

        stringstream s;
        for ( int idx = 0 ; idx < len ; ++idx ) {
            s << asString(nthcar(strs, idx));
        }

        cont->run(makeString(s.str()));
    };

    validate(name, args, cont, callback, matcher);
}

void scam::applyString2List(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string->list" };
    Matcher mStr   = matchString("string");
    Matcher mStart = matchStartIndex("start", "string");
    Matcher mEnd   = matchEndIndex("end", "string", "start");
    Matcher matcher = matchAlternative(name,
                                       matchSequence(name, mStr, mStart, mEnd),
                                       matchSequence(name, mStr, mStart),
                                       mStr);

    Callback callback = [cont] (const ValidatorResult & result) -> void
    {
        const string str = asString(result.get("string"));
        const int start = valueOrDefault(result.get("start"), 0);
        const int end   = valueOrDefault(result.get("end"), str.size());

        string newStr =
            start == end ? string("") : str.substr(start, end-start);
        vector<ScamValue> chars;

        for ( const char c : newStr ) {
            chars.push_back(makeCharacter(c));
        }

        cont->run(makeList(chars));
    };

    validate(name, args, cont, callback, matcher);
}

extern void scam::applyList2String(ScamValue args,
                                   Continuation * cont,
                                   ScamEngine * engine)
{
    static const char * name { "string->list" };
    Matcher matcher =
        matchSublist("chars", "cs", matchCount("cs", "c", matchCharacter("c")));

    Callback callback = [cont] (const ValidatorResult & result) -> void
    {
        ScamValue chars = result.get("chars");
        const int len = length(chars);

        stringstream s;
        for ( int idx = 0 ; idx < len ; ++idx ) {
            s << asChar(nthcar(chars, idx));
        }

        cont->run(makeString(s.str()));
    };

    validate(name, args, cont, callback, matcher);
}

void scam::applyStringCopy(ScamValue args,
                           Continuation * cont,
                           ScamEngine * engine)
{
    static const char * name { "string-copy" };
    Matcher mStr   = matchString("string");
    Matcher mStart = matchStartIndex("start", "string");
    Matcher mEnd   = matchEndIndex("end", "string", "start");
    Matcher matcher = matchAlternative(name,
                                       matchSequence(name, mStr, mStart, mEnd),
                                       matchSequence(name, mStr, mStart),
                                       mStr);

    Callback callback = [cont] (const ValidatorResult & result) -> void
    {
        const string str = asString(result.get("string"));
        const int start = valueOrDefault(result.get("start"), 0);
        const int end   = valueOrDefault(result.get("end"), str.size());

        string newStr =
            start == end ? string("") : str.substr(start, end-start);

        cont->run(makeString(newStr));
    };

    validate(name, args, cont, callback, matcher);
}

void scam::applyStringCopyX(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string-copy!" };

    Matcher mTo     = matchString("to");
    Matcher mAt     = matchIndex("at", "to");
    Matcher mFrom   = matchString("from");
    Matcher mStart  = matchStartIndex("start", "from");
    Matcher mEnd    = matchEndIndex("end", "from", "start");
    Matcher mAlt5   = matchSequence(name, mTo, mAt, mFrom, mStart, mEnd);
    Matcher mAlt4   = matchSequence(name, mTo, mAt, mFrom, mStart);
    Matcher mAlt3   = matchSequence(name, mTo, mAt, mFrom);
    Matcher matcher = matchAlternative(name, mAlt5, mAlt4, mAlt3);

    Callback callback = [cont] (const ValidatorResult & result) -> void
    {
        ScamValue toValue = result.get("to");
        if ( isImmutable(toValue) ) {
            ScamValue err =
                makeErrorExtended(name,
                                  ": Cannot mutate constant string ",
                                  writeValue(toValue));
            cont->run(err);
            return;
        }

        string toStr = asString(toValue);
        const int at = asInteger(result.get("at"));
        const string fromStr = asString(result.get("from"));
        const int start = valueOrDefault(result.get("start"), 0);
        const int end   = valueOrDefault(result.get("end"), fromStr.size());

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
    };

    validate(name, args, cont, callback, matcher);
}

void scam::applyStringFillX(ScamValue args,
                            Continuation * cont,
                            ScamEngine * engine)
{
    static const char * name { "string-fill!" };

    Matcher mString = matchString("string");
    Matcher mFill   = matchCharacter("fill");
    Matcher mStart  = matchStartIndex("start", "string");
    Matcher mEnd    = matchEndIndex("end", "string", "start");
    Matcher mAlt4   = matchSequence(name, mString, mFill, mStart, mEnd);
    Matcher mAlt3   = matchSequence(name, mString, mFill, mStart);
    Matcher mAlt2   = matchSequence(name, mString, mFill);
    Matcher matcher = matchAlternative(name, mAlt4, mAlt3, mAlt2);

    Callback callback = [cont] (const ValidatorResult & result) -> void
    {
        ScamValue strValue = result.get("string");
        if ( isImmutable(strValue) ) {
            ScamValue err =
                makeErrorExtended(name,
                                  ": Cannot mutate constant string ",
                                  writeValue(strValue));
            cont->run(err);
            return;
        }

        string toStr = asString(strValue);
        const char fill = asChar(result.get("fill"));
        const int start = valueOrDefault(result.get("start"), 0);
        const int end   = valueOrDefault(result.get("end"), toStr.size());

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
    };

    validate(name, args, cont, callback, matcher);
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
        Matcher matcher = matchCount(name, "string", matchString("string"), 2);

        Callback callback =
            [=] (const ValidatorResult & result) -> void
        {
            ScamValue strs = result.get(name);
            int len = length(strs);
            bool rv = true;

            string prev = transform(asString(nthcar(strs, 0)));
            for ( int idx = 1 ; (idx < len) && rv ; ++idx ) {
                const string curr = transform(asString(nthcar(strs, idx)));
                rv = compare(prev, curr);
                prev = curr;
            }

            cont->run(makeBoolean(rv));
        };

        validate(name, args, cont, callback, matcher);
    }

    void transformString(ScamValue args,
                         Continuation * cont,
                         const char * name,
                         Transformer transformer)
    {
        {
            Matcher matcher = matchString("arg");
            Callback callback =
                [cont, &transformer] (const ValidatorResult & result) -> void
            {
                const string text = asString(result.get("arg"));
                string newText = transformer(text);
                cont->run(makeString(newText));
            };

            validate(name, args, cont, callback, matcher);
        }
    }

    int valueOrDefault(ScamValue src, int defaultValue)
    {
        if ( isNull(src) ) {
            return defaultValue;
        }

        return asInteger(src);
    }

}
