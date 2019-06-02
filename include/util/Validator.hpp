#if ! defined(VALIDATOR_HPP)
#define VALIDATOR_HPP 1

#include "ScamFwd.hpp"

#include <functional>
#include <map>
#include <string>

namespace scam
{
    class ValidatorResult
    {
    public:
        ValidatorResult();

        operator bool() const;
        void fail();
        void set(const std::string & key, ScamValue value);
        ScamValue get(const std::string & key) const;

    private:
        bool status;
        std::map<const std::string, ScamValue> parameters;
    };

    struct MatcherControl
    {
        const std::string & context;
        ScamValue           args;
        Continuation      * cont;
        ValidatorResult   & results;
    };

    using Callback = std::function<void(const ValidatorResult &)>;

    using Matcher = std::function<ScamValue(MatcherControl & control)>;

    void validate(const std::string & context,
                  ScamValue args,
                  Continuation * cont,
                  Callback callback);

    void validate(const std::string & context,
                  ScamValue args,
                  Continuation * cont,
                  Callback callback,
                  Matcher matcher);

    Matcher matchCharacter(const std::string & key);
    Matcher matchInteger(const std::string & key);
    Matcher matchNonNegativeInteger(const std::string & key);
    Matcher matchString(const std::string & key);
    Matcher matchSymbol(const std::string & key);

    Matcher matchIndex(const std::string & key, const std::string & itemKey);
    
    Matcher matchSequence(const std::string & key, Matcher m1, Matcher m2);

    template <typename ... Args>
    Matcher matchSequence(const std::string & key,
                          Matcher m1,
                          Matcher m2,
                          Args && ... rest)
    {
        return matchSequence(key, m1, matchSequence(key, m2, rest...));
    }

    Matcher matchAlternative(const std::string & key, Matcher m1, Matcher m2);

    template <typename ... Args>
    Matcher matchAlternative(const std::string & key,
                             Matcher m1,
                             Matcher m2,
                             Args && ... rest)
    {
        return matchAlternative(key, m1, matchAlternative(key, m2, rest...));
    }

    Matcher matchCount(const std::string & key,
                       const std::string & itemKey,
                       Matcher m,
                       unsigned min = 0,
                       unsigned max = 9999u);

    Matcher matchSublist(const std::string & key,
                         const std::string & itemKey,
                         Matcher m);
}

#endif
