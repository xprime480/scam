#if ! defined(SEQUENCEOPS_HPP)
#define SEQUENCEOPS_HPP 1

#include "ScamFwd.hpp"

#include <cstdlib>

namespace scam
{
#if 0
    extern ScamValue getCar(ScamValue value);
    extern ScamValue getCdr(ScamValue value);
    extern size_t length(ScamValue value);
    extern ScamValue nthcar(ScamValue value, size_t n);
    extern ScamValue nthcdr(ScamValue value, size_t n);
#endif
    
    // not like scheme append ... returns a copy of lst with value
    // at the end
    //
    extern ScamValue append(ScamValue lst, ScamValue value);
}

#endif
