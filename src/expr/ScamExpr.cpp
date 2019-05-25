#include "expr/ScamExpr.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"

#include <sstream>
#include <utility>

using namespace scam;
using namespace std;

ScamExpr::ScamExpr(unsigned long type, bool managed)
    : ScamData(type, managed)
{
}

void ScamExpr::eval(Continuation * cont, Env * env) const
{
    ScamExpr * argggh = const_cast<ScamExpr *>(this);
    cont->run(argggh);
}

bool ScamExpr::hasApply() const
{
    return 0 != (type & ScamData::Applicable);
}

void ScamExpr::apply(ScamValue args, Continuation * cont, Env * env)
{
    ScamValue err =
        ExpressionFactory::makeError("Not possible to apply <",
                                     writeValue(this),
                                     "> to args ",
                                     writeValue(args));
    cont->run(err);
}

void ScamExpr::mapEval(Continuation * cont, Env * env) const
{
    ScamExpr * hack = const_cast<ScamExpr *>(this);
    cont->run(hack);
}

ScamValue ScamExpr::withEnvUpdate(Env * updated) const
{
    stringstream s;
    s << "Cannot update env of <" << writeValue(this) << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

bool ScamExpr::equals(ConstScamValue expr) const
{
    return this == expr;
}

