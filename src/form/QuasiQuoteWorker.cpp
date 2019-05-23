#include "form/QuasiQuoteWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamExpr.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/TypePredicates.hpp"
#include "form/QQSpliceCont.hpp"
#include "form/QQConsListCarCont.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

static const char * myName = "quasiquote";

QuasiQuoteWorker::QuasiQuoteWorker(ScamValue form,
                                   Continuation * cont,
                                   Env * env)
    : Worker(myName)
    , form(form)
    , cont(cont)
    , env(env)
{
}

QuasiQuoteWorker *
QuasiQuoteWorker::makeInstance(ScamValue form, Continuation * cont, Env * env)
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

bool QuasiQuoteWorker::verify_single_form(ScamValue input, Continuation * cont)
{
    if ( ! TypePredicates::isList(input) || 1 != input->length() ) {
        failedArgParseMessage(myName, "form", input, cont);
        return false;
    }
    return true;
}

void QuasiQuoteWorker::unquote_form(ScamValue input, Continuation * cont)
{
    if ( verify_single_form(input, cont) ) {
        ScamValue form = input->nthcar(0);
        form->eval(cont, env);
    }
}

void QuasiQuoteWorker::splice_form(ScamValue input, Continuation * cont)
{
    if ( verify_single_form(input, cont) ) {
        Continuation * h =
            standardMemoryManager.make<QQSpliceCont>(cont);

        ScamValue form = input->nthcar(0);
        form->eval(h, env);
    }
}

void QuasiQuoteWorker::cons_qq_list(ScamValue car,
                                    ScamValue cdr,
                                    Continuation * cont)
{
    Continuation * h =
        standardMemoryManager.make<QQConsListCarCont>(cdr, cont, env);
    build_qq_form(car, h);
}

void QuasiQuoteWorker::build_qq_list(ScamValue input, Continuation * cont)
{
    ScamValue first = input->nthcar(0);
    ScamValue rest  = input->nthcdr(0);

    const bool isSym = TypePredicates::isSymbol(first);
    string const sym = writeValue(first);
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

void QuasiQuoteWorker::build_qq_form(ScamValue input, Continuation * cont)
{
    if ( ! TypePredicates::isList(input) || TypePredicates::isNil(input) ) {
        cont->run(input);
    }
    else {
        build_qq_list(input, cont);
    }
}

