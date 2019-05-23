#include "expr/ScamExpr.hpp"

#include "Continuation.hpp"
#include "ScamException.hpp"
#include "expr/ExprWriter.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/TypePredicates.hpp"

#include <sstream>
#include <utility>

#include "util/DebugTrace.hpp"

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
                                     ExprWriter::write(this),
                                     "> to args ",
                                     ExprWriter::write(args));
    cont->run(err);
}

void ScamExpr::mapEval(Continuation * cont, Env * env) const
{
    ScamExpr * argggh = const_cast<ScamExpr *>(this);
    cont->run(argggh);
}

char ScamExpr::toChar() const
{
    stringstream s;
    s << "Cannot convert <" << ExprWriter::write(this) << "> to character";
    throw ScamException(s.str());

    return '\0';
}

double ScamExpr::asDouble() const
{
    return ScamNumeric::asDouble(this);
}

pair<int, int> ScamExpr::asRational() const
{
    return ScamNumeric::asRational(this);
}

int ScamExpr::asInteger() const
{
    return ScamNumeric::asInteger(this);
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
    s << "Cannot take car of <" << ExprWriter::write(this) << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

ScamValue ScamExpr::getCdr() const
{
    stringstream s;
    s << "Cannot take cdr of <" << ExprWriter::write(this) << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

size_t ScamExpr::length() const
{
    stringstream s;
    s << "Cannot take the length of <" << ExprWriter::write(this) << ">";
    throw ScamException(s.str());

    return 0u;
}

ScamValue ScamExpr::nthcar(size_t n) const
{
    stringstream s;
    s << "Cannot index <" << ExprWriter::write(this) << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

ScamValue ScamExpr::nthcdr(size_t n) const
{
    stringstream s;
    s << "Cannot index <" << ExprWriter::write(this) << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

ScamValue ScamExpr::withEnvUpdate(Env * updated) const
{
    stringstream s;
    s << "Cannot update env of <" << ExprWriter::write(this) << ">";
    throw ScamException(s.str());

    return ExpressionFactory::makeNull();
}

void ScamExpr::setSelf(ScamValue expr) const
{
    stringstream s;
    s << "Cannot set self of <" << ExprWriter::write(this) << ">";
    throw ScamException(s.str());
}

void ScamExpr::setParent(ScamValue expr) const
{
    stringstream s;
    s << "Cannot set parent of <" << ExprWriter::write(this) << ">";
    throw ScamException(s.str());
}

bool ScamExpr::equals(ConstScamValue expr) const
{
    return this == expr;
}

