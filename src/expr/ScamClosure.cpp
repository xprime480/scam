
#include "expr/ScamClosure.hpp"

#include "Continuation.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

ScamClosure::ScamClosure(ExprHandle const & args,
                         ExprHandle const & forms,
                         Env & env)
    : args(args)
    , forms(forms)
    , env(env)
{
}

string ScamClosure::toString() const
{
    stringstream s;
    s << "(proc " << args->toString() << " " << forms->toString() << ")";
    return s.str();
}

bool ScamClosure::hasApply() const
{
    return true;
}

void ScamClosure::apply(ExprHandle const & args, ContHandle cont, Env & env)
{
    forms->getCar()->eval(cont, this->env);
}

bool ScamClosure::isProcedure() const
{
    return true;
}
