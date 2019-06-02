#if ! defined(STRINGOPS_HPP)
#define STRINGOPS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    extern void applyStringUpcase(ScamValue args,
                                  Continuation * cont,
                                  ScamEngine * engine);
    extern void applyStringDowncase(ScamValue args,
                                    Continuation * cont,
                                    ScamEngine * engine);
}

#endif
