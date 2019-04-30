#if ! defined(DEFINEBACKTRACKER_HPP)
#define DEFINEBACKTRACKER_HPP 1

#include "Backtracker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class DefineBacktracker : public Backtracker
    {
    private:
        friend class scam::MemoryManager;

        DefineBacktracker(ExprHandle sym,
                          Env * env,
                          Backtracker * backtracker);

        static DefineBacktracker *
        makeInstance(ExprHandle sym,
                     Env * env,
                     Backtracker * backtracker);

    public:
        void mark() const override;
        void run() override;

    private:
        ExprHandle sym;
        Env      * env;
    };
}

#endif
