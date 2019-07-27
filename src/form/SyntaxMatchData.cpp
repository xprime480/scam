#include "form/SyntaxMatchData.hpp"

#include "ScamException.hpp"
#include "expr/ValueWriter.hpp"

#include <sstream>

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
        overflow();
    }
}

void SyntaxMatchVariable::append(const SyntaxMatchVariable & newData)
{
    if ( ellipsis ) {
        data.insert(data.end(), newData.data.begin(), newData.data.end());
    }
    else {
        overflow();
    }
}

bool SyntaxMatchVariable::isEllipsis() const
{
    return ellipsis;
}

int SyntaxMatchVariable::count() const
{
    return data.size();
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

string SyntaxMatchVariable::identify() const
{
    stringstream s;
    string sep = "";

    s << "[";
    for ( const auto item : data ) {
        s << sep;
        s << writeValue(item);
        sep = ", ";
    }
    s << "]";

    return s.str();
}

void SyntaxMatchVariable::overflow()
{
    static const char * text
    { "internal error: adding multiple values to simple syntax variable" };

    throw ScamException(text);
}

void SyntaxMatchData::clear()
{
    data.clear();
}

void SyntaxMatchData::add(string identifier, bool ellipsis, ScamValue value)
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

void SyntaxMatchData::append(const SyntaxMatchData & newData)
{
    for ( const auto that : newData.data ) {
        const auto & identifier = that.first;
        const auto iter = data.find(identifier);
        if ( data.end() == iter ) {
            auto value = make_pair(identifier, that.second);
            data.insert(value);
        }
        else {
            iter->second.append(that.second);
        }
    }
}

bool SyntaxMatchData::hasEllipsisId(const string & identifier) const
{
    bool rv = false;

    auto iter = data.find(identifier);
    if ( data.end() != iter ) {
        rv = iter->second.isEllipsis();
    }

    return rv;
}

int SyntaxMatchData::count(const string & identifier) const
{
    int rv = 0;

    auto iter = data.find(identifier);
    if ( data.end() != iter ) {
        rv = iter->second.count();
    }

    return rv;
}

ScamValue SyntaxMatchData::get(string identifier, unsigned n) const
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

string SyntaxMatchData::identify() const
{
    stringstream s;
    string sep = "";

    s << "{";
    for ( const auto item : data ) {
        s << sep;
        s << item.first;
        const auto & var = item.second;
        if ( var.isEllipsis() ) {
            s << "...";
        }
        s << " ";
        s << var.identify();
        sep = ", ";
    }
    s << "}";

    return s.str();
}
