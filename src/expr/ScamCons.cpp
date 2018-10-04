
#include "expr/ScamCons.hpp"

#include "Continuation.hpp"
#include "ScamContext.hpp"

#include "expr/ExpressionFactory.hpp"

#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    class Evaluator : public Continuation
    {
    public:
        Evaluator(shared_ptr<ScamExpr> args, ScamContext & context)
            : args(args)
            , context(context)
        {
        }

        void run(shared_ptr<ScamExpr> e) const override
        {
            e->apply(args, context);
        }

    private:
        shared_ptr<ScamExpr> args;
        ScamContext context;
    };
}

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
    shared_ptr<Continuation> newCont = make_shared<Evaluator>(cdr, context);
    ScamContext newContext = context;
    newContext.cont = newCont;
    car->eval(newContext);
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

shared_ptr<ScamExpr> ScamCons::getCar() const
{
    return car;
}

shared_ptr<ScamExpr> ScamCons::getCdr() const
{
    return cdr;
}

size_t ScamCons::length() const
{
    size_t len = 1;
    if ( cdr->isCons() ) {
        len += cdr->length();
    }
    else if ( ! cdr->isNil() ) {
        len += 1;
    }

    return len;
}

std::shared_ptr<ScamExpr> ScamCons::nth(size_t n) const
{
    auto f = [=] () -> std::shared_ptr<ScamExpr> {
        stringstream s;
        s << "Index " << n << " requested for " << toString();
        return ExpressionFactory::makeError(s.str());
    };

    std::shared_ptr<ScamExpr> rv;

    if ( 0 == n ) {
        rv = car;
    }
    else if ( cdr->isCons() ) {
        rv = cdr->nth(n-1);
        if ( rv->error() ) {
            rv = f();
        }
    }
    else if ( cdr->isNil() || n > 1 ) {
        rv = f();
    }
    else {
        rv = cdr;
    }

    return rv;
}

shared_ptr<ScamExpr> ScamCons::clone()
{
    return ExpressionFactory::makeCons(car, cdr);
}
