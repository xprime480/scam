#include "form/QuasiQuoteWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "form/QQConsListCarCont.hpp"
#include "form/QQSpliceCont.hpp"
#include "util/ArgListHelper.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;
using namespace std;

static const char * myName = "quasiquote";

QuasiQuoteWorker::QuasiQuoteWorker(ScamValue form,
                                   Continuation * cont,
                                   Env * env,
                                   ScamEngine * engine)
    : Worker(myName, engine)
    , form(form)
    , cont(cont)
    , env(env)
{
}

QuasiQuoteWorker * QuasiQuoteWorker::makeInstance(ScamValue form,
                                                  Continuation * cont,
                                                  Env * env,
                                                  ScamEngine * engine)
{
    return new QuasiQuoteWorker(form, cont, env, engine);
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
    if ( ! isList(input) || 1 != length(input) ) {
        failedArgParseMessage(myName, "form", input, cont);
        return false;
    }
    return true;
}

void QuasiQuoteWorker::unquote_form(ScamValue input, Continuation * cont)
{
    if ( verify_single_form(input, cont) ) {
        ScamValue form = nthcar(input, 0);
        eval(form, cont, env, engine);
    }
}

void QuasiQuoteWorker::splice_form(ScamValue input, Continuation * cont)
{
    if ( verify_single_form(input, cont) ) {
        Continuation * h =
            standardMemoryManager.make<QQSpliceCont>(cont, engine);
        ScamValue form = nthcar(input, 0);
        eval(form, h, env, engine);
    }
}

void QuasiQuoteWorker::cons_qq_list(ScamValue car,
                                    ScamValue cdr,
                                    Continuation * cont)
{
    Continuation * h =
        standardMemoryManager.make<QQConsListCarCont>(cdr,
                                                      cont,
                                                      env,
                                                      engine);
    build_qq_form(car, h);
}

void QuasiQuoteWorker::build_qq_list(ScamValue input, Continuation * cont)
{
    ScamValue first = nthcar(input, 0);
    ScamValue rest  = nthcdr(input, 0);

    const bool isSym = isSymbol(first);
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
    if ( ! isList(input) || isNull(input) ) {
        cont->run(input);
    }
    else {
        build_qq_list(input, cont);
    }
}
