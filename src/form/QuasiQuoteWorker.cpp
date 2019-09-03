#include "form/QuasiQuoteWorker.hpp"

#include "Continuation.hpp"
#include "env/Env.hpp"
#include "expr/EvalOps.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/TypePredicates.hpp"
#include "expr/ValueWriter.hpp"
#include "form/QQConsListCarCont.hpp"
#include "form/QQSpliceCont.hpp"
#include "util/MemoryManager.hpp"
#include "util/Parameter.hpp"


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

QuasiQuoteWorker * QuasiQuoteWorker::makeInstance(ScamValue form,
                                                  Continuation * cont,
                                                  Env * env)
{
    return new QuasiQuoteWorker(form, cont, env);
}

void QuasiQuoteWorker::mark()
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
    ObjectParameter p0;
    return argsToParms(input, myName, p0);
}

void QuasiQuoteWorker::unquote_form(ScamValue input, Continuation * cont)
{
    if ( verify_single_form(input, cont) ) {
        ScamValue form = nthcar(input, 0);
        eval(form, cont, env);
    }
}

void QuasiQuoteWorker::splice_form(ScamValue input, Continuation * cont)
{
    if ( verify_single_form(input, cont) ) {
        Continuation * h =
            standardMemoryManager.make<QQSpliceCont>(cont);
        ScamValue form = nthcar(input, 0);
        eval(form, h, env);
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
        cont->handleValue(input);
    }
    else {
        build_qq_list(input, cont);
    }
}
