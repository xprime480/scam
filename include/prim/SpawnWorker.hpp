#if ! defined(SPAWNWORKER_HPP)
#define SPAWNWORKER_HPP 1

#include "Worker.hpp"

namespace scam
{
    class MemoryManager;
    class Continuation;

    class SpawnWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        SpawnWorker(Continuation * cont, bool value);

        static SpawnWorker *
        makeInstance(Continuation * cont, bool value);

    public:
        void mark() override;
        void run() override;

    private:
        Continuation * cont;
        bool           value;
    };
}

#endif
