#include "expr/WorkerData.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"

using namespace scam;
using namespace std;

WorkerData::WorkerData(ScamExpr * car,
                       ScamExpr * cdr,
                       Continuation * original,
                       Env * env)
    : car(car)
    , cdr(cdr)
    , original(original)
    , cont(nullptr)
    , env(env)
{
}

void WorkerData::mark() const
{
    car->mark();
    cdr->mark();
    original->mark();
    if ( cont ) {
        cont->mark();
    }
    env->mark();
}