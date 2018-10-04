#include "expr/ScamVector.hpp"

#include "Extractor.hpp"
#include "ScamContext.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamVector::ScamVector(vector<shared_ptr<ScamExpr>> elts)
    : elts(elts)
{
}

string ScamVector::toString() const
{
    stringstream s;
    string sep { "" };

    s << "[";
    for ( auto const & e : elts ) {
        s << sep << e->toString();
        sep = " ";
    }
    s << "]";

    return s.str();
}

void ScamVector::eval(ScamContext & context)
{
    ScamContext newContext = context;
    shared_ptr<Extractor> newCont = make_shared<Extractor>();
    newContext.cont = newCont;

    vector<shared_ptr<ScamExpr>> evaled;
    for ( auto const & e : elts ) {
        e->eval(newContext);
        shared_ptr<ScamExpr> expr = newCont->getExpr();
        if ( expr->error() ) {
            context.cont->run(expr);
            return;
        }
        else {
            evaled.push_back(expr);
        }
    }

    shared_ptr<ScamExpr> final = ExpressionFactory::makeVector(evaled);
    context.cont->run(final);
}

bool ScamVector::isVector() const
{
    return true;
}

size_t ScamVector::length() const
{
    return elts.size();
}

shared_ptr<ScamExpr> ScamVector::nth(size_t n) const
{
    if ( n >= length() ) {
        stringstream s;
        s << "Requested index " << n
          << " of a " << length() << "-element vector";
        return ExpressionFactory::makeError(s.str());
    }

    return elts[n];
}

shared_ptr<ScamExpr> ScamVector::clone()
{
    return ExpressionFactory::makeVector(elts);
}
