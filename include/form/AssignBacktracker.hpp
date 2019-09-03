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
                          Backtracker * backtracker);

        static AssignBacktracker * makeInstance(ScamValue sym,
                                                ScamValue old,
                                                Env * env,
                                                Backtracker * backtracker);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue   sym;
        ScamValue   old;
        Env       * env;
    };
}

#endif
