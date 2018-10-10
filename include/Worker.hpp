#if ! defined(SCAM_WORKER_H)
#define SCAM_WORKER_H

#include <memory>
#include <string>

namespace scam
{
    class Worker;

    using WorkerHandle = std::shared_ptr<Worker>;

    class Worker
    {
    public:
        Worker(char const * id);
        virtual ~Worker();

        virtual void run();
        std::string id() const;

    private:
        std::string const name;
        static std::string makeName(char const * id);
    };
}

#endif
