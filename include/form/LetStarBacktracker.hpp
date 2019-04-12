#if ! defined(LETSTARBACKTRACKER_HPP)
#define LETSTARBACKTRACKER_HPP 1

#include "Backtracker.hpp"

namespace scam
{
    class MemoryManager;
    class Env;
    class ScamExpr;

    class LetStarBacktracker : public Backtracker
    {
    private:
        friend class scam::MemoryManager;

        LetStarBacktracker(Env * env,
                           ScamExpr * sym,
                           Backtracker * backtracker);

        static LetStarBacktracker * makeInstance(Env * env,
                                                 ScamExpr * sym,
                                                 Backtracker * backtracker);

    public:
        void mark() const override;
        void run() override;

    private:
        Env *      env;
        ScamExpr * sym;
    };
}

#endif
