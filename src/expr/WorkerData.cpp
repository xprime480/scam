#include "expr/WorkerData.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamData.hpp"

using namespace scam;
using namespace std;

WorkerData::WorkerData(ScamValue car,
                       ScamValue cdr,
                       Continuation * original,
                       Env * env)
    : car(car)
    , cdr(cdr)
    , original(original)
    , cont(nullptr)
    , env(env)
{
}

void WorkerData::mark()
{
    car->mark();
    cdr->mark();
    original->mark();
    if ( cont ) {
        cont->mark();
    }
    env->mark();
}
