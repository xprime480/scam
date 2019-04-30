#if ! defined(LETSTARWORKER_HPP)
#define LETSTARWORKER_HPP 1

#include "LetBaseWorker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LetStarWorker : public LetBaseWorker
    {
    private:
        friend class scam::MemoryManager;
        LetStarWorker(ExprHandle args,
                      Continuation * cont,
                      Env * env,
                      ScamEngine * engine);

        static LetStarWorker * makeInstance(ExprHandle args,
                                            Continuation * cont,
                                            Env * env,
                                            ScamEngine * engine);

    protected:
        void do_next(ExprHandle formals,
                     ExprHandle values,
                     ExprHandle forms) override;

    private:
        ScamEngine * engine;
    };
}

#endif
