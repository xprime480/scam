
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
                         Env * env,
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

void Amb::apply(ScamExpr * args, Continuation * cont, Env * env)
{
    do_apply(args, cont, env, engine);
}

namespace
{
    class AmbBacktracker : public Backtracker
    {
    private:
        friend class scam::MemoryManager;

        AmbBacktracker(ScamExpr * args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine,
                       Backtracker * parent)
            : Backtracker("AmbBacktracker", parent)
            , args(args)
            , cont(cont)
            , env(env)
            , engine(engine)
        {
        }

        static AmbBacktracker * makeInstance(ScamExpr * args,
                                             Continuation * cont,
                                             Env * env,
                                             ScamEngine * engine,
                                             Backtracker * parent)
        {
            return new AmbBacktracker(args, cont, env, engine, parent);
        }

    public:
        void mark() const override
        {
            if ( ! isMarked() ) {
                Backtracker::mark();
                args->mark();
                cont->mark();
                env->mark();
            }
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

                (void) engine->getBacktracker();
                Backtracker * newBt =
                    standardMemoryManager.make<AmbBacktracker>(tail,
                                                               cont,
                                                               env,
                                                               engine,
                                                               getParent());

                engine->setBacktracker(newBt);

                head->eval(cont, env);
            }
        }

    private:
        ScamExpr      * args;
        Continuation  * cont;
        Env           * env;
        ScamEngine    * engine;
    };

    void do_apply(ScamExpr * args,
                  Continuation * cont,
                  Env * env,
                  ScamEngine * engine)
    {
        Backtracker * backtracker = engine->getBacktracker();
        Backtracker * newBt =
            standardMemoryManager.make<AmbBacktracker>(args,
                                                       cont,
                                                       env,
                                                       engine,
                                                       backtracker);
        newBt->run();
    }
}
