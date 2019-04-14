#include "form/LetBaseWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

LetBaseWorker::LetBaseWorker(char const * name,
                             ScamExpr * args,
                             Continuation * cont,
                             Env * env)
    : Worker(name)
    , cont(cont)
    , env(env)
    , args(args)
{
}

void LetBaseWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        args->mark();
        cont->mark();
        env->mark();
    }
}

void LetBaseWorker::run()
{
    Worker::run();

    if ( ! verify_args() ) {
        return;
    }

    ScamExpr * parsed  = parse_args();
    ScamExpr * formals = parsed->getCar()->getCar();
    ScamExpr * values  = parsed->getCar()->getCdr();
    ScamExpr * forms   = parsed->getCdr();

    do_next(formals, values, forms);
}

void LetBaseWorker::report_error()
{
    ScamExpr * err =
        ExpressionFactory::makeError("Expected (((sym form)...) forms...);",
                                     " got ",
                                     args->toString());

    cont->run(err);
}

bool LetBaseWorker::verify_single(ScamExpr * arg)
{
    if ( ! arg->isList() || 2 != arg->length() ) {
        report_error();
        return false;
    }

    if ( ! arg->getCar()->isSymbol() ) {
        report_error();
        return false;
    }

    return true;
}

bool LetBaseWorker::verify_list(ScamExpr * check)
{
    if ( check->isNil() ) {
        return true;
    }

    if ( ! check->isList() ) {
        report_error();
        return false;
    }

    if ( ! verify_single(check->getCar()) ) {
        return false;
    }

    return verify_list(check->getCdr());
}

bool LetBaseWorker::verify_args()
{
    if ( ! args->isList() || args->isNil() ) {
        report_error();
        return false;
    }

    ScamExpr * check = args->getCar();
    return verify_list(check);
}

ScamExpr * LetBaseWorker::parse_bindings(ScamExpr * bindings)
{
    if ( bindings->isNil() ) {
        ScamExpr * nil = ExpressionFactory::makeNil();
        return ExpressionFactory::makeCons(nil, nil);
    }

    ScamExpr * one  = bindings->getCar();
    ScamExpr * rest = bindings->getCdr();

    ScamExpr * separated = parse_bindings(rest);

    ScamExpr * symList
        = ExpressionFactory::makeCons(one->getCar(),
                                      separated->getCar());
    ScamExpr * valList
        = ExpressionFactory::makeCons(one->getCdr()->getCar(),
                                      separated->getCdr());

    return ExpressionFactory::makeCons(symList, valList);
}

ScamExpr * LetBaseWorker::parse_args()
{
    ScamExpr * forms    = args->getCdr();
    ScamExpr * bindings = args->getCar();

    ScamExpr * separated = parse_bindings(bindings);

    return ExpressionFactory::makeCons(separated, forms);
}

