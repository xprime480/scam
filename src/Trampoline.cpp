
#include "Trampoline.hpp"

#include "WorkQueue.hpp"
#include "Worker.hpp"

using namespace scam;

void scam::Trampoline(WorkQueue & queue)
{
    while ( ! queue.empty() ) {
        Worker * worker = queue.get();
        if ( worker ) {
            worker->run();
        }
    }
}
