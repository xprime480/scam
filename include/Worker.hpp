#if ! defined(SCAM_WORKER_H)
#define SCAM_WORKER_H

#include <memory>

namespace scam
{
    class Worker;

    using WorkerHandle = std::shared_ptr<Worker>;

    class Worker
    {
    public:
        virtual ~Worker() {}

	virtual void run() = 0;
    };
}

#endif
