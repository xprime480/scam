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

    using Matcher = std::function<ScamValue(const std::string & context,
                                            ScamValue args,
                                            Continuation * cont,
                                            ValidatorResult & results)>;

    ValidatorResult validate(const std::string & context,
                             ScamValue args,
                             Continuation * cont);

    ValidatorResult validate(const std::string & context,
                             ScamValue args,
                             Continuation * cont,
                             Matcher matcher);

    Matcher matchString(const std::string & key);
    Matcher matchInteger(const std::string & key);
}

#endif
