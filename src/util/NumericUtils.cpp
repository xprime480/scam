#include "util/NumericUtils.hpp"

#include <cstdlib>

using namespace scam;
using namespace std;

namespace impl
{
    extern int gcdInternal(int a, int b);
}

int scam::gcd(int a, int b)
{
    if ( 0 == a ) {
        return b;
    }
   if ( (a < 0) ^ (b < 0) ) {
        return gcd(abs(a), abs(b));
    }
    if ( a < b ) {
        return impl::gcdInternal(b, a);
    }
    return impl::gcdInternal(a, b);
}

int impl::gcdInternal(int a, int b)
{
    if ( 0 == b ) {
        return a;
    }
    int newB = a % b;
    int newA = b;
    return impl::gcdInternal(newA, newB);
}
