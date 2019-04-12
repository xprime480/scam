#if ! defined(AMBBACKTRACKER_HPP)
#define AMBBACKTRACKER_HPP 1

#include "Backtracker.hpp"

namespace scam
{
    class ScamExpr;
    class Continuation;
    class Env;
    class ScamEngine;
    class MemoryManager;

    class AmbBacktracker : public Backtracker
    {
    private:
        friend class scam::MemoryManager;

        AmbBacktracker(ScamExpr * args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine,
                       Backtracker * parent);

        static AmbBacktracker * makeInstance(ScamExpr * args,
                                             Continuation * cont,
                                             Env * env,
                                             ScamEngine * engine,
                                             Backtracker * parent);

    public:
        void mark() const override;
        void run() override;

    private:
        ScamExpr      * args;
        Continuation  * cont;
        Env           * env;
        ScamEngine    * engine;
    };

}

#endif
