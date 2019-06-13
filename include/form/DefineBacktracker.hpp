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

        DefineBacktracker(ScamValue sym,
                          Env * env,
                          Backtracker * backtracker,
                          ScamEngine * engine);

        static DefineBacktracker * makeInstance(ScamValue sym,
                                                Env * env,
                                                Backtracker * backtracker,
                                                ScamEngine * engine);

    public:
        void mark() const override;
        void run() override;

    private:
        ScamValue    sym;
        Env        * env;
        ScamEngine * engine;
    };
}

#endif
