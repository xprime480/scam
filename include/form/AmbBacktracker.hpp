#if ! defined(AMBBACKTRACKER_HPP)
#define AMBBACKTRACKER_HPP 1

#include "Backtracker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class AmbBacktracker : public Backtracker
    {
    private:
        friend class scam::MemoryManager;

        AmbBacktracker(ExprHandle args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine,
                       Backtracker * parent);

        static AmbBacktracker * makeInstance(ExprHandle args,
                                             Continuation * cont,
                                             Env * env,
                                             ScamEngine * engine,
                                             Backtracker * parent);

    public:
        void mark() const override;
        void run() override;

    private:
        ExprHandle      args;
        Continuation  * cont;
        Env           * env;
        ScamEngine    * engine;
    };

}

#endif
