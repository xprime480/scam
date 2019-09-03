#if ! defined(SCAM_WORKQUEUE_H)
#define SCAM_WORKQUEUE_H

#include "ScamEngine.hpp"

#include <deque>

namespace scam
{
    class Worker;

    class WorkQueue
    {
    public:
        void put(Worker * worker);
        Worker * get();
        bool empty() const;
        void mark();

    private:
        std::deque<Worker *> workers;
    };

    extern WorkQueue GlobalWorkQueue;

    template <typename T, typename... Args>
    T * workQueueHelper(Args &&... args)
    {
        MemoryManager & mm = ScamEngine::getEngine().getMemoryManager();
        T * worker = mm.make<T>(args...);
        GlobalWorkQueue.put(worker);
        return worker;
    }
}

#endif
