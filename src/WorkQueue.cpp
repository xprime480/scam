
#include "WorkQueue.hpp"

using namespace scam;

void WorkQueue::put(std::shared_ptr<Worker> & worker)
{
    workers.emplace_back(std::shared_ptr<Worker>(worker));
}

std::shared_ptr<Worker> WorkQueue::get()
{
    static std::shared_ptr<Worker> lazy;
    if ( empty() ) {
        return lazy;
    }

    std::shared_ptr<Worker> worker = workers.front();
    workers.pop_front();
    return worker;
}

bool WorkQueue::empty() const
{
    return workers.empty();
}

WorkQueue scam::GlobalWorkQueue;
