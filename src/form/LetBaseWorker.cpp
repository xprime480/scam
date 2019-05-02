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

    ExprHandle parsed  = parse_args();
    ExprHandle formals = parsed->getCar()->getCar();
    ExprHandle values  = parsed->getCar()->getCdr();
    ExprHandle forms   = parsed->getCdr();

    do_next(formals, values, forms);
}

ExprHandle LetBaseWorker::parse_bindings()
{
    ExprHandle nil = ExpressionFactory::makeNil();
    std::vector<ExprHandle> syms;
    std::vector<ExprHandle> vals;

    const size_t count = parser->getBindingCount();

    for ( size_t idx = 0 ; idx < count ; ++idx ) {
        BindFormParser * bf = parser->getBinding(idx);

	ScamSymbol * sym = const_cast<ScamSymbol *>(bf->getSymbol());
        syms.push_back(sym);

        ExprHandle valForm = bf->getForm();
        if ( nullptr == valForm ) {
            vals.push_back(nil);
        }
        else {
            vals.push_back(valForm);
        }
    }

    ExprHandle symList = ExpressionFactory::makeList(syms);
    ExprHandle valList = ExpressionFactory::makeList(vals);

    return ExpressionFactory::makeCons(symList, valList);
}

ExprHandle LetBaseWorker::parse_args()
{
    ExprHandle forms     = parser->getForms();
    ExprHandle separated = parse_bindings();

    return ExpressionFactory::makeCons(separated, forms);
}

