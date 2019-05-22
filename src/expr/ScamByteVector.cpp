#include "expr/ScamByteVector.hpp"

#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamByteVector::ScamByteVector(ByteVec const & elts)
    : ScamExpr(ScamData::ByteVector)
{
    BYTEVECTORP(data) = new remove_reference<decltype(BYTEVECTOR(data))>::type;
    BYTEVECTOR(data) = elts;
}

ScamByteVector::~ScamByteVector()
{
    delete BYTEVECTORP(data);
}

ScamByteVector * ScamByteVector::makeInstance(ByteVec const & elts)
{
    return new ScamByteVector(elts);
}

string ScamByteVector::toString() const
{
    stringstream s;
    string sep { "" };

    s << "#u8(";
    for ( auto const & e : BYTEVECTOR(data) ) {
        s << sep << (int)e;
        sep = " ";
    }
    s << ")";

    return s.str();
}

size_t ScamByteVector::length() const
{
    return BYTEVECTOR(data).size();
}

ExprHandle ScamByteVector::nthcar(size_t n) const
{
    if ( n >= length() ) {
        return ExpressionFactory::makeError("Requested index ",
                                            n,
                                            " of a ",
                                            length(),
                                            "-element byteVector");
    }

    return ExpressionFactory::makeInteger(BYTEVECTOR(data)[n], true);
}

bool ScamByteVector::equals(ConstExprHandle expr) const
{
    if ( ! expr->isByteVector() ) {
        return false;
    }
    ScamByteVector const * that = dynamic_cast<ScamByteVector const *>(expr);
    if ( BYTEVECTOR(data).size() != BYTEVECTOR(that->data).size() ) {
        return false;
    }
    for ( size_t idx = 0 ; idx < BYTEVECTOR(data).size() ; ++idx ) {
        if ( BYTEVECTOR(data)[idx] != BYTEVECTOR(that->data)[idx] ) {
            return false;
        }
    }

    return true;
}
