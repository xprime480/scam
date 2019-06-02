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
        newValue->makeMutable();
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
        matchSequence(name, matchString("string"), matchIndex("k", "string"));

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
}
