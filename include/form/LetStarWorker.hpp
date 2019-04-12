#if ! defined(LETSTARWORKER_HPP)
#define LETSTARWORKER_HPP 1

#include "LetBaseWorker.hpp"

namespace scam
{
    class MemoryManager;
    class Continuation;
    class Env;
    class ScamExpr;
    class ScamEngine;

    class LetStarWorker : public LetBaseWorker
    {
    private:
        friend class scam::MemoryManager;
        LetStarWorker(ScamExpr * args,
                      Continuation * cont,
                      Env * env,
                      ScamEngine * engine);

        static LetStarWorker * makeInstance(ScamExpr * args,
                                            Continuation * cont,
                                            Env * env,
                                            ScamEngine * engine);

    protected:
        void do_next(ScamExpr * formals,
                     ScamExpr * values,
                     ScamExpr * forms) override;

    private:
        ScamEngine * engine;
    };
}

#endif
