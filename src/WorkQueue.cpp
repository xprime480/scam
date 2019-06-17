
#include "WorkQueue.hpp"
#include "Worker.hpp"

using namespace scam;

void WorkQueue::put(Worker * worker)
{
    workers.push_back(worker);
}

Worker * WorkQueue::get()
{
    if ( empty() ) {
        return nullptr;
    }

    Worker * worker = workers.front();
    workers.pop_front();
    return worker;
}

bool WorkQueue::empty() const
{
    return workers.empty();
}

void WorkQueue::mark()
{
    for ( const auto w : workers ) {
        w->mark();
    }
}

WorkQueue scam::GlobalWorkQueue;
