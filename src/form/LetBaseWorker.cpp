#include "form/LetBaseWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ScamData.hpp"
#include "expr/SequenceOps.hpp"
#include "expr/ValueFactory.hpp"
#include "input/BindFormParser.hpp"
#include "input/LetParser.hpp"

using namespace scam;
using namespace std;

LetBaseWorker::LetBaseWorker(char const * name,
                             LetParser * parser,
                             Continuation * cont,
                             Env * env)
    : Worker(name)
    , cont(cont)
    , env(env)
    , parser(parser)
{
}

void LetBaseWorker::mark() const
{
    if ( ! isMarked() ) {
        Worker::mark();
        parser->mark();
        cont->mark();
        env->mark();
    }
}

void LetBaseWorker::run()
{
    Worker::run();

    ScamValue parsed  = parse_args();
    ScamValue formals = getCar(getCar(parsed));
    ScamValue values  = getCdr(getCar(parsed));
    ScamValue forms   = getCdr(parsed);

    do_next(formals, values, forms);
}

ScamValue LetBaseWorker::parse_bindings()
{
    ScamValue nil = makeNil();
    std::vector<ScamValue> syms;
    std::vector<ScamValue> vals;

    const size_t count = parser->getBindingCount();

    for ( size_t idx = 0 ; idx < count ; ++idx ) {
        BindFormParser * bf = parser->getBinding(idx);

        ScamValue sym = bf->getSymbol();
        syms.push_back(sym);

        ScamValue valForm = bf->getForm();
        if ( nullptr == valForm ) {
            vals.push_back(nil);
        }
        else {
            vals.push_back(valForm);
        }
    }

    ScamValue symList = makeList(syms);
    ScamValue valList = makeList(vals);

    return makeCons(symList, valList);
}

ScamValue LetBaseWorker::parse_args()
{
    ScamValue forms     = parser->getForms();
    ScamValue separated = parse_bindings();

    return makeCons(separated, forms);
}
