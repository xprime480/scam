
#include "Trampoline.hpp"

#include "WorkQueue.hpp"
#include "Worker.hpp"
#include "util/MemoryManager.hpp"

using namespace scam;

void scam::Trampoline(WorkQueue & queue)
{
    static MemoryManager & mm = standardMemoryManager;
    static size_t count { 100 };

    while ( ! queue.empty() ) {
        Worker * worker = queue.get();
        if ( worker ) {
            worker->run();
        }
	if ( 0 == --count ) {
	    mm.gc();
	    count = 100;
	}
    }
}
