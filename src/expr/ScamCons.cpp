
#include "expr/ScamCons.hpp"

#include "ScamContext.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

ScamCons::ScamCons(shared_ptr<ScamExpr> car, shared_ptr<ScamExpr> cdr)
    : car(car)
    , cdr(cdr)
{
}

string ScamCons::toString() const
{
    stringstream s;
    s << "(";
    s << car->toString();
    shared_ptr<ScamExpr> next = cdr;
    while ( ! next->isNil() ) {
        if ( next->isCons() ) {
            s << " " << next->getCar()->toString();
            next = next->getCdr();
        }
        else {
            s << " . " << next->toString();
            break;
        }
    }
    s << ")";

    return s.str();
}

void ScamCons::eval(ScamContext & context)
{
    static string const msg { "ScamCons::eval NOT IMPLEMENTED" };
    static shared_ptr<ScamExpr> err = ExpressionFactory::makeError(msg);
    context.cont->run(err);
}

bool ScamCons::isCons() const
{
    return true;
}

bool ScamCons::isList() const
{
    if ( cdr->isNil() ) {
        return true;
    }
    if ( ! cdr->isCons() ) {
        return false;
    }
    return cdr->isList();
}

std::shared_ptr<ScamExpr> ScamCons::getCar() const
{
    return car;
}

std::shared_ptr<ScamExpr> ScamCons::getCdr() const
{
    return cdr;
}

shared_ptr<ScamExpr> ScamCons::clone()
{
    return ExpressionFactory::makeCons(car, cdr);
}
