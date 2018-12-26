
#include "form/Amb.hpp"

#include "Backtracker.hpp"
#include "Continuation.hpp"
#include "ScamEngine.hpp"
#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "expr/ExpressionFactory.hpp"

#include <iostream>
#include <sstream>

using namespace scam;
using namespace std;

namespace
{
    extern void
    do_apply(ScamExpr * args, ContHandle cont, Env env, ScamEngine * engine);
}

Amb::Amb(ScamEngine * engine)
    : SpecialForm("amb")
    , engine(engine)
{
}

void Amb::apply(ScamExpr * args, ContHandle cont, Env env)
{
    do_apply(args, cont, env, engine);
}

namespace
{
    class AmbBacktracker : public Backtracker
    {
    public:
        AmbBacktracker(ScamExpr * args,
                       ContHandle cont,
                       Env env,
                       ScamEngine * engine,
                       BacktrackHandle parent)
            : Backtracker("AmbBacktracker", parent)
            , args(args->clone())
            , cont(cont)
            , env(env)
            , engine(engine)
        {
        }

        void run() override
        {
            Backtracker::run();

            if ( 0 == args->length() ) {
                runParent(cont);
            }
            else {
                ExprHandle head = args->nthcar(0);
                ExprHandle tail = args->nthcdr(0);

                BacktrackHandle backtracker = engine->getBacktracker();
                shared_ptr<Backtracker> newBt =
                    make_shared<AmbBacktracker>(tail.get(),
                                                cont,
                                                env,
                                                engine,
                                                getParent());
                engine->setBacktracker(newBt);

                head->eval(cont, env);
            }
        }

    private:
        ExprHandle      args;
        ContHandle      cont;
        Env             env;
        ScamEngine    * engine;
    };

    void
    do_apply(ScamExpr * args, ContHandle cont, Env env, ScamEngine * engine)
    {
        BacktrackHandle backtracker = engine->getBacktracker();
        shared_ptr<Backtracker> newBt =
            make_shared<AmbBacktracker>(args, cont, env, engine, backtracker);
        newBt->run();
    }
}
