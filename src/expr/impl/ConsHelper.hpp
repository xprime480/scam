#if ! defined(CONS_HELPER_HPP)
# define CONS_HELPER_HPP 1

#include "Continuation.hpp"
#include "env/Env.hpp"
#include "expr/ScamData.hpp"

namespace scam
{
    namespace cons_impl
    {
        void scamConsEvalHelper(ScamValue car,
                                ScamValue cdr,
                                Continuation * cont,
                                Env * env);

        void scamConsMapHelper(ScamValue car,
                               ScamValue cdr,
                               Continuation * cont,
                               Env * env);
    }
}

#endif

