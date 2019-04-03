#if ! defined(SCAM_WORKQUEUE_H)
#define SCAM_WORKQUEUE_H

#include "Worker.hpp"

#include <deque>
#include <memory>

namespace scam
{
    class Continuation;
    class Env;
    class ScamExpr;

    using ContHandle = std::shared_ptr<Continuation> ;
    using ExprHandle = std::shared_ptr<ScamExpr>;

    class WorkQueue
    {
    public:
        void put(std::shared_ptr<Worker> & worker);
        std::shared_ptr<Worker> get();

        bool empty() const;

    private:
        std::deque<std::shared_ptr<Worker>> workers;
    };

    extern WorkQueue GlobalWorkQueue;

    template <typename T, typename... Args>

    typename std::shared_ptr<T> workQueueHelper(Args &&... args)
    {
        std::shared_ptr<T> thunk = std::make_shared<T>(args...);
        WorkerHandle start = thunk;
        GlobalWorkQueue.put(start);
        return thunk;
    }
}

#endif
