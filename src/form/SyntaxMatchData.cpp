#include "form/SyntaxMatchData.hpp"

#include "ScamException.hpp"

using namespace scam;
using namespace std;

SyntaxMatchVariable::SyntaxMatchVariable(bool ellipsis)
    : ellipsis(ellipsis)
{
}

void SyntaxMatchVariable::add(ScamValue value)
{
    if ( ellipsis || data.empty() ) {
        data.push_back(value);
    }
    else {
        static const char * text
        { "internal error: adding second value to simple syntax variable" };

        throw ScamException(text);
    }
}

ScamValue SyntaxMatchVariable::get(unsigned n) const
{
    if ( ! ellipsis ) {
        n = 0;
    }

    if ( n >= data.size() ) {
        static const char * text
        { "internal error: fetching syntax variable out of range" };

        throw ScamException(text);
    }

    return data.at(n);
}

void SyntaxMatchData::add(std::string identifier,
                          bool ellipsis,
                          ScamValue value)
{
    auto iter = data.find(identifier);

    if ( data.end() == iter ) {
        SyntaxMatchVariable var(ellipsis);
        auto value = make_pair(identifier, var);
        data.insert(value);
        iter = data.find(identifier);
    }

    iter->second.add(value);
}

ScamValue SyntaxMatchData::get(std::string identifier, unsigned n) const
{
    auto iter = data.find(identifier);

    if ( data.end() == iter ) {
        static const char * text
        { "internal error: syntax variable does not exist" };

        throw ScamException(text);
    }

    ScamValue rv = iter->second.get(n);;
    return rv;
}
