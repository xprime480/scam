#if ! defined(DEFINEBACKTRACKER_HPP)
#define DEFINEBACKTRACKER_HPP 1

#include "Backtracker.hpp"

namespace scam
{
    class ScamExpr;
    class Env;
    class MemoryManager;

    class DefineBacktracker : public Backtracker
    {
    private:
        friend class scam::MemoryManager;

        DefineBacktracker(ScamExpr * sym,
                          Env * env,
                          Backtracker * backtracker);

        static DefineBacktracker *
        makeInstance(ScamExpr * sym,
                     Env * env,
                     Backtracker * backtracker);

    public:
        void mark() const override;
        void run() override;

    private:
        ScamExpr *      sym;
        Env *           env;
    };
}

#endif
