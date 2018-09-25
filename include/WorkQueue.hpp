#if ! defined(SCAM_WORKQUEUE_H)
#define SCAM_WORKQUEUE_H

#include <deque>
#include <memory>

namespace scam
{
    class Worker;

    class WorkQueue
    {
    public:
        void put(std::shared_ptr<Worker> & worker);
        std::shared_ptr<Worker> get();

        bool empty() const;

    private:
        std::deque<std::shared_ptr<Worker>> workers;
    };
}

#endif
