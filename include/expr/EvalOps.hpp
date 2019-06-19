#if ! defined(EVALOPS_HPP)
#define EVALOPS_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    void eval(ScamValue value,
              Continuation * cont,
              Env * env,
              ScamEngine * engine);

    void apply(ScamValue value,
               ScamValue args,
               Continuation * cont,
               Env * env,
               ScamEngine * engine);

    Env * env(ScamValue value);

    void mapEval(ScamValue value,
                 Continuation * cont,
                 Env * env,
                 ScamEngine * engine);

    ScamValue withEnvUpdate(ScamValue value, Env * updated);
}

#endif
