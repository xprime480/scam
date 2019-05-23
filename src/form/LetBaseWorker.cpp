#include "form/LetBaseWorker.hpp"

#include "Continuation.hpp"
#include "Env.hpp"
#include "expr/ExpressionFactory.hpp"
#include "expr/ScamExpr.hpp"
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
    ScamValue formals = parsed->getCar()->getCar();
    ScamValue values  = parsed->getCar()->getCdr();
    ScamValue forms   = parsed->getCdr();

    do_next(formals, values, forms);
}

ScamValue LetBaseWorker::parse_bindings()
{
    ScamValue nil = ExpressionFactory::makeNil();
    std::vector<ScamValue> syms;
    std::vector<ScamValue> vals;

    const size_t count = parser->getBindingCount();

    for ( size_t idx = 0 ; idx < count ; ++idx ) {
        BindFormParser * bf = parser->getBinding(idx);

	ScamSymbol * sym = const_cast<ScamSymbol *>(bf->getSymbol());
        syms.push_back(sym);

        ScamValue valForm = bf->getForm();
        if ( nullptr == valForm ) {
            vals.push_back(nil);
        }
        else {
            vals.push_back(valForm);
        }
    }

    ScamValue symList = ExpressionFactory::makeList(syms);
    ScamValue valList = ExpressionFactory::makeList(vals);

    return ExpressionFactory::makeCons(symList, valList);
}

ScamValue LetBaseWorker::parse_args()
{
    ScamValue forms     = parser->getForms();
    ScamValue separated = parse_bindings();

    return ExpressionFactory::makeCons(separated, forms);
}

