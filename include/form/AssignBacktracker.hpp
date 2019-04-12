#if ! defined(ASSIGNBACKTRACKER_HPP)
#define ASSIGNBACKTRACKER_HPP 1

#include "Backtracker.hpp"

namespace scam
{
    class ScamExpr;
    class Env;
    class ScamEngine;
    class MemoryManager;

    class AssignBacktracker : public Backtracker
    {
    private:
        friend class scam::MemoryManager;

        AssignBacktracker(ScamExpr * sym,
                          ScamExpr * old,
                          Env * env,
                          Backtracker * backtracker);

        static AssignBacktracker * makeInstance(ScamExpr * sym,
                                                ScamExpr * old,
                                                Env * env,
                                                Backtracker * backtracker);

    public:
        void mark() const override;
        void run() override;

    private:
        ScamExpr *      sym;
        ScamExpr *      old;
        Env *           env;
    };
}

#endif
