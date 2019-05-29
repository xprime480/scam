#if ! defined(PRIMITIVE_CARCDR_HPP)
#define PRIMITIVE_CARCDR_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern ScamValue carCdrCommon(ScamValue args,
                                  Continuation * cont,
                                  const char * name);
}

#endif
