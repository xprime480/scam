#if ! defined(LETSTARBACKTRACKER_HPP)
#define LETSTARBACKTRACKER_HPP 1

#include "Backtracker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LetStarBacktracker : public Backtracker
    {
    private:
        friend class scam::MemoryManager;

        LetStarBacktracker(Env * env, ScamValue sym, Backtracker * backtracker);

        static LetStarBacktracker *
	makeInstance(Env * env, ScamValue sym, Backtracker * backtracker);

    public:
        void mark() override;
        void run() override;

    private:
        Env        * env;
        ScamValue    sym;
    };
}

#endif
