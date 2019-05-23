#include "expr/ScamToInternal.hpp"

#include "ScamException.hpp"
#include "expr/ExprWriter.hpp"
#include "expr/TypePredicates.hpp"

#include <sstream>

using namespace scam;
using namespace std;

char scam::asChar(const ScamData * data)
{
    if ( ! TypePredicates::isChar(data) ) {
        stringstream s;
        s << "Cannot convert <" << ExprWriter::write(data) << "> to character";
        throw ScamException(s.str());
    }

    return CHARVAL(data);
}

double scam::asDouble(const ScamData * data)
{
    if ( ! TypePredicates::isReal(data) ) {
        stringstream s;
        s << "Cannot convert <" << ExprWriter::write(data) << "> to double";
        throw ScamException(s.str());
    }

    if ( TypePredicates::isInteger(data) ) {
        return (double) INTVAL(data);
    }
    else if ( TypePredicates::isRational(data) ) {
        return ((double) NUMPART(data) / (double) DENPART(data) );
    }
    else if ( TypePredicates::isNaN(data) ||
              TypePredicates::isNegInf(data) ||
              TypePredicates::isPosInf(data) ) {
        // drop through to error case;
    }
    else if ( TypePredicates::isReal(data) ) {
        return REALVAL(data);
    }

    return 0.0;
}

RationalPair scam::asRational(const ScamData * data)
{
    if ( ! TypePredicates::isRational(data) ) {
        stringstream s;
        s << "Cannot convert <" << ExprWriter::write(data) << "> to rational";
        throw ScamException(s.str());
    }

    RationalPair pair { 0, 1 };

    if ( TypePredicates::isInteger(data) ) {
        pair.num = INTVAL(data);
    }
    else {
        pair.num = NUMPART(data);
        pair.den = DENPART(data);
    }

    return pair;
}

int scam::asInteger(const ScamData * data)
{
    if ( ! TypePredicates::isInteger(data) ) {
        stringstream s;
        s << "Cannot convert <" << ExprWriter::write(data) << "> to integer";
        throw ScamException(s.str());
    }

    return INTVAL(data);
}
