#include "expr/ScamVector.hpp"

#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/VectorWorker.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamVector::ScamVector(ExprVec const & elts)
    : elts(elts)
{
}

ScamVector * ScamVector::makeInstance(ExprVec const & elts)
{
    return new ScamVector(elts);
}

void ScamVector::mark() const
{
    if ( ! isMarked() ) {
        ScamExpr::mark();
        for ( auto const & e : elts ) {
            e->mark();
        }
    }
}

string ScamVector::toString() const
{
    stringstream s;
    string sep { "" };

    s << "#(";
    for ( auto const & e : elts ) {
        s << sep << e->toString();
        sep = " ";
    }
    s << ")";

    return s.str();
}

void ScamVector::eval(Continuation * cont, Env * env)
{
    workQueueHelper<VectorWorker>(cont, env, elts);
}

bool ScamVector::isVector() const
{
    return true;
}

size_t ScamVector::length() const
{
    return elts.size();
}

ScamExpr * ScamVector::nthcar(size_t n) const
{
    if ( n >= length() ) {
        return ExpressionFactory::makeError("Requested index ",
                                            n,
                                            " of a ",
                                            length(),
                                            "-element vector");
    }

    return elts[n];
}

bool ScamVector::equals(ScamExpr const * expr) const
{
    if ( ! expr->isVector() ) {
        return false;
    }
    ScamVector const * that = dynamic_cast<ScamVector const *>(expr);
    if ( elts.size() != that->elts.size() ) {
        return false;
    }
    for ( size_t idx = 0 ; idx < elts.size() ; ++idx ) {
        if ( ! elts[idx]->equals(that->elts[idx]) ) {
            return false;
        }
    }

    return true;
}
