
#include "ScamException.hpp"

using namespace scam;
using namespace std;

ScamException::ScamException(string const & msg)
    : msg(msg)
{
}

string const & ScamException::getMessage() const
{
    return msg;
}

