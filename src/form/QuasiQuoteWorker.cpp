#include "form/QuasiQuoteWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "form/QQSpliceCont.hpp"
#include "form/QQConsListCarCont.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

static const char * myName = "quasiquote";

QuasiQuoteWorker::QuasiQuoteWorker(ExprHandle form,
                                   Continuation * cont,
                                   Env * env)
    : Worker(myName)
    , form(form)
    , cont(cont)
    , env(env)
{
}

QuasiQuoteWorker *
QuasiQuoteWorker::makeInstance(ExprHandle form, Continuation * cont, Env * env)
{
    return new QuasiQuoteWorker(form, cont, env);
}

void QuasiQuoteWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        form->mark();
        cont->mark();
        env->mark();
    }
}

void QuasiQuoteWorker::run()
{
    build_qq_form(form, cont);
}

bool QuasiQuoteWorker::verify_single_form(ExprHandle input, Continuation * cont)
{
    if ( ! input->isList() || 1 != input->length() ) {
        failedArgParseMessage(myName, "form", input, cont);
        return false;
    }
    return true;
}

void QuasiQuoteWorker::unquote_form(ExprHandle input, Continuation * cont)
{
    if ( verify_single_form(input, cont) ) {
        ExprHandle form = input->nthcar(0);
        form->eval(cont, env);
    }
}

void QuasiQuoteWorker::splice_form(ExprHandle input, Continuation * cont)
{
    if ( verify_single_form(input, cont) ) {
        Continuation * h =
            standardMemoryManager.make<QQSpliceCont>(cont);

        ExprHandle form = input->nthcar(0);
        form->eval(h, env);
    }
}

void QuasiQuoteWorker::cons_qq_list(ExprHandle car,
                                    ExprHandle cdr,
                                    Continuation * cont)
{
    Continuation * h =
        standardMemoryManager.make<QQConsListCarCont>(cdr, cont, env);
    build_qq_form(car, h);
}

void QuasiQuoteWorker::build_qq_list(ExprHandle input, Continuation * cont)
{
    ExprHandle first = input->nthcar(0);
    ExprHandle rest  = input->nthcdr(0);

    const bool isSym = first->isSymbol();
    string const sym = ExprWriter::write(first);
    if ( isSym && sym == "unquote" ) {
        unquote_form(rest, cont);
    }
    else if ( isSym && sym == "splice" ) {
        splice_form(rest, cont);
    }
    else {
        cons_qq_list(first, rest, cont);
    }
}

void QuasiQuoteWorker::build_qq_form(ExprHandle input, Continuation * cont)
{
    if ( ! input->isList() || input->isNil() ) {
        cont->run(input);
    }
    else {
        build_qq_list(input, cont);
    }
}

