#include "expr/ScamExpr.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"
#include "expr/ExpressionFactory.hpp"
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
    ScamExpr * argggh = const_cast<ScamExpr *>(this);
    cont->run(argggh);
}

ConstScamValue ScamExpr::realPart() const
{
    // temporary hack!!!
    ConstScamValue rv = ScamNumeric::realPart(this);
    if ( TypePredicates::isNull(rv) ) {
        rv = this;
    }
    return rv;
}

ConstScamValue ScamExpr::imagPart() const
{
    return ScamNumeric::imagPart(this);
}

ScamValue ScamExpr::getCar() const
{
    stringstream s;
    s << "Cannot take car of <" << writeValue(this) << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

ScamValue ScamExpr::getCdr() const
{
    stringstream s;
    s << "Cannot take cdr of <" << writeValue(this) << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

size_t ScamExpr::length() const
{
    stringstream s;
    s << "Cannot take the length of <" << writeValue(this) << ">";
    throw ScamException(s.str());

    return 0u;
}

ScamValue ScamExpr::nthcar(size_t n) const
{
    stringstream s;
    s << "Cannot index <" << writeValue(this) << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

ScamValue ScamExpr::nthcdr(size_t n) const
{
    stringstream s;
    s << "Cannot index <" << writeValue(this) << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

ScamValue ScamExpr::withEnvUpdate(Env * updated) const
{
    stringstream s;
    s << "Cannot update env of <" << writeValue(this) << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

void ScamExpr::setSelf(ScamValue expr) const
{
    stringstream s;
    s << "Cannot set self of <" << writeValue(this) << ">";
    throw ScamException(s.str());
}

void ScamExpr::setParent(ScamValue expr) const
{
    stringstream s;
    s << "Cannot set parent of <" << writeValue(this) << ">";
    throw ScamException(s.str());
}

bool ScamExpr::equals(ConstScamValue expr) const
{
    return this == expr;
}

