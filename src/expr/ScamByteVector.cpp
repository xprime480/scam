#include "expr/ScamByteVector.hpp"

#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/VectorWorker.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamByteVector::ScamByteVector(ByteVec const & elts)
    : elts(elts)
{
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
    for ( auto const & e : elts ) {
        s << sep << (int)e;
        sep = " ";
    }
    s << ")";

    return s.str();
}

bool ScamByteVector::isByteVector() const
{
    return true;
}

size_t ScamByteVector::length() const
{
    return elts.size();
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

    return ExpressionFactory::makeInteger(elts[n], true);
}

bool ScamByteVector::equals(ConstExprHandle expr) const
{
    if ( ! expr->isByteVector() ) {
        return false;
    }
    ScamByteVector const * that = dynamic_cast<ScamByteVector const *>(expr);
    if ( elts.size() != that->elts.size() ) {
        return false;
    }
    for ( size_t idx = 0 ; idx < elts.size() ; ++idx ) {
        if ( elts[idx] != that->elts[idx] ) {
            return false;
        }
    }

    return true;
}
