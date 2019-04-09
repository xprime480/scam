
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
    extern void do_apply(ScamExpr * args,
                         Continuation * cont,
                         Env env,
                         ScamEngine * engine);
}

Amb::Amb(ScamEngine * engine)
    : SpecialForm("amb", true)
    , engine(engine)
{
}

Amb * Amb::makeInstance(ScamEngine * engine)
{
    return new Amb(engine);
}

void Amb::apply(ScamExpr * args, Continuation * cont, Env env)
{
    do_apply(args, cont, env, engine);
}

namespace
{
    class AmbBacktracker : public Backtracker
    {
    public:
        AmbBacktracker(ScamExpr * args,
                       Continuation * cont,
                       Env env,
                       ScamEngine * engine,
                       BacktrackHandle parent)
            : Backtracker("AmbBacktracker", parent)
            , args(args)
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
                ScamExpr * head = args->nthcar(0);
                ScamExpr * tail = args->nthcdr(0);

                BacktrackHandle backtracker = engine->getBacktracker();
                shared_ptr<Backtracker> newBt =
                    make_shared<AmbBacktracker>(tail,
                                                cont,
                                                env,
                                                engine,
                                                getParent());
                engine->setBacktracker(newBt);

                head->eval(cont, env);
            }
        }

    private:
        ScamExpr *      args;
        Continuation *  cont;
        Env             env;
        ScamEngine    * engine;
    };

    void
    do_apply(ScamExpr * args, Continuation * cont, Env env, ScamEngine * engine)
    {
        BacktrackHandle backtracker = engine->getBacktracker();
        shared_ptr<Backtracker> newBt =
            make_shared<AmbBacktracker>(args, cont, env, engine, backtracker);
        newBt->run();
    }
}
