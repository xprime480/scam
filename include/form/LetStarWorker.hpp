#if ! defined(LETSTARWORKER_HPP)
#define LETSTARWORKER_HPP 1

#include "LetBaseWorker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LetParser;
    
    class LetStarWorker : public LetBaseWorker
    {
    private:
        friend class scam::MemoryManager;
        LetStarWorker(LetParser * parser,
                      Continuation * cont,
                      Env * env,
                      ScamEngine * engine);

        static LetStarWorker * makeInstance(LetParser * parser,
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
