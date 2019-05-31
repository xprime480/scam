#include "input/charStream.hpp"

#include "ScamException.hpp"

using namespace scam;
using namespace std;

namespace
{
    extern void checkSequence(const PositionType & lhs,
                              const PositionType & rhs);
}


bool scam::operator==(const PositionType & lhs, const PositionType & rhs)
{
    checkSequence(lhs, rhs);
    return lhs.offset == rhs.offset;
}

bool scam::operator!=(const PositionType & lhs, const PositionType & rhs)
{
    return ! (lhs == rhs);
}

bool scam::operator<(const PositionType & lhs, const PositionType & rhs)
{
    checkSequence(lhs, rhs);
    return lhs.offset < rhs.offset;
}

bool scam::operator>(const PositionType & lhs, const PositionType & rhs)
{
    checkSequence(lhs, rhs);
    return lhs.offset > rhs.offset;
}

bool scam::operator<=(const PositionType & lhs, const PositionType & rhs)
{
    return ! (lhs > rhs);
}

bool scam::operator>=(const PositionType & lhs, const PositionType & rhs)
{
    return ! (lhs < rhs);
}

namespace
{
    void checkSequence(const PositionType & lhs, const PositionType & rhs)
    {
        if ( lhs.sequence != rhs.sequence ) {
            static const string msg =
                "cannot compare positions from different sequences";
            throw ScamException(msg);
        }
    }
}
