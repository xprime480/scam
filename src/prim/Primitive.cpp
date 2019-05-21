#include "prim/Primitive.hpp"

#include "WorkQueue.hpp"
#include "prim/PrimWorker.hpp"

#include <sstream>

using namespace scam;
using namespace std;

Primitive::Primitive(string const & name)
    : name(name)
{
    data.type = ScamData::Primitive;
}

string Primitive::toString() const
{
    stringstream s;
    s << "Primitive " << name;
    return s.str();
}

bool Primitive::hasApply() const
{
    return true;
}

void Primitive::apply(ExprHandle args, Continuation * cont, Env * env)
{
    /*
     * For primitives, the argument confirmation is delegated to the
     * derived class' applyArgs function.
     */
    workQueueHelper<PrimWorker>(cont, env, args, this);
}
