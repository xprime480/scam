#include "form/LetBaseWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"

using namespace scam;
using namespace std;

LetBaseWorker::LetBaseWorker(char const * name,
                             ExprHandle args,
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

    ExprHandle parsed  = parse_args();
    ExprHandle formals = parsed->getCar()->getCar();
    ExprHandle values  = parsed->getCar()->getCdr();
    ExprHandle forms   = parsed->getCdr();

    do_next(formals, values, forms);
}

void LetBaseWorker::report_error()
{
    ExprHandle err =
        ExpressionFactory::makeError("Expected (((sym form)...) forms...);",
                                     " got ",
                                     args->toString());

    cont->run(err);
}

bool LetBaseWorker::verify_single(ExprHandle arg)
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

bool LetBaseWorker::verify_list(ExprHandle check)
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

    ExprHandle check = args->getCar();
    return verify_list(check);
}

ExprHandle LetBaseWorker::parse_bindings(ExprHandle bindings)
{
    if ( bindings->isNil() ) {
        ExprHandle nil = ExpressionFactory::makeNil();
        return ExpressionFactory::makeCons(nil, nil);
    }

    ExprHandle one  = bindings->getCar();
    ExprHandle rest = bindings->getCdr();

    ExprHandle separated = parse_bindings(rest);

    ExprHandle symList
        = ExpressionFactory::makeCons(one->getCar(),
                                      separated->getCar());
    ExprHandle valList
        = ExpressionFactory::makeCons(one->getCdr()->getCar(),
                                      separated->getCdr());

    return ExpressionFactory::makeCons(symList, valList);
}

ExprHandle LetBaseWorker::parse_args()
{
    ExprHandle forms    = args->getCdr();
    ExprHandle bindings = args->getCar();

    ExprHandle separated = parse_bindings(bindings);

    return ExpressionFactory::makeCons(separated, forms);
}

