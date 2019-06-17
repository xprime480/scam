#if ! defined(ASSIGNBACKTRACKER_HPP)
#define ASSIGNBACKTRACKER_HPP 1

#include "Backtracker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class AssignBacktracker : public Backtracker
    {
    private:
        friend class scam::MemoryManager;

        AssignBacktracker(ScamValue sym,
                          ScamValue old,
                          Env * env,
                          Backtracker * backtracker,
                          ScamEngine * engine);

        static AssignBacktracker * makeInstance(ScamValue sym,
                                                ScamValue old,
                                                Env * env,
                                                Backtracker * backtracker,
                                                ScamEngine * engine);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue   sym;
        ScamValue   old;
        Env       * env;
        ScamEngine * engine;
    };
}

#endif
