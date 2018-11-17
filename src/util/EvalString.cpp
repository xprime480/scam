
#include "util/EvalString.hpp"

#include "ScamException.hpp"
#include "Trampoline.hpp"
#include "WorkQueue.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>

using namespace scam;
using namespace std;

EvalString::EvalString(ScamEngine * engine, std::string const & text)
    : engine(engine)
    , tokenizer(text)
    , cont(make_shared<Extractor>())
{
    engine->pushInput(tokenizer);
}

EvalString::~EvalString()
{
    engine->popInput();
}

ExprHandle EvalString::getLast()
{
    vector<ExprHandle> exprs;
    try {
        getAll(exprs, true);
        if ( exprs.empty() ) {
            return ExpressionFactory::makeNull();
        }
        return exprs.back();
    }
    catch ( ScamException e ) {
        return ExpressionFactory::makeError(e.getMessage());
    }
}

void EvalString::getAll(vector<ExprHandle> & exprs,  bool stopOnError)
{
    for ( ;; ) {
        ExprHandle expr = getNext();
        if ( expr->isNull() ) {
            break;
        }
        exprs.push_back(expr);
        if ( stopOnError && expr->error() ) {
            break;
        }
    }
}

ExprHandle EvalString::getNext()
{
    ExprHandle expr = engine->read();
    if ( expr->isNull() || expr->error() ) {
        return expr;
    }
    engine->eval(expr.get(), cont);
    Trampoline(GlobalWorkQueue);
    ExprHandle rv = cont->getExpr();
    return rv;
}
