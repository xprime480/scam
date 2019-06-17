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

        AmbBacktracker(ScamValue args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine,
                       Backtracker * parent);

        static AmbBacktracker * makeInstance(ScamValue args,
                                             Continuation * cont,
                                             Env * env,
                                             ScamEngine * engine,
                                             Backtracker * parent);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue      args;
        Continuation  * cont;
        Env           * env;
        ScamEngine    * engine;
    };

}

#endif
