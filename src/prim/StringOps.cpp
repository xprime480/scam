#include "prim/StringOps.hpp"

#include "Continuation.hpp"
#include "expr/ScamData.hpp"
#include "expr/ScamToInternal.hpp"
#include "expr/ValueFactory.hpp"
#include "input/ListParser.hpp"
#include "util/Validator.hpp"

#include <algorithm>
#include <cctype>

using namespace scam;
using namespace std;

namespace
{
    using Transformer = function<int(int)>;

    extern char upcaseChar(char c);
    extern char downcaseChar(char c);

    extern void transformByChar(ScamValue args,
                                Continuation * cont,
                                const char * name,
                                Transformer transformer);
}

void scam::applyStringUpcase(ScamValue args,
                             Continuation * cont,
                             ScamEngine * engine)
{
    transformByChar(args, cont, "string-upcase", upcaseChar);
}

void scam::applyStringDowncase(ScamValue args,
                               Continuation * cont,
                               ScamEngine * engine)
{
    transformByChar(args, cont, "string-downcase", downcaseChar);
}

namespace
{
    char upcaseChar(char c) { return toupper(c); }
    char downcaseChar(char c) { return tolower(c); }

    void transformByChar(ScamValue args,
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
                string newText = text;
                auto t = [&transformer](const char c) -> char
                {
                    return transformer(c);
                };
                transform(text.begin(), text.end(), newText.begin(), t);
                cont->run(makeString(newText));
            };

            validate(name, args, cont, callback, matcher);
        }
    }
}
