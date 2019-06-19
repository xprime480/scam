#if ! defined(SCAM_WORKER_H)
#define SCAM_WORKER_H

#include "util/ManagedObject.hpp"

#include "ScamFwd.hpp"

#include <string>

namespace scam
{
    class MemoryManager;

    class Worker : public ManagedObject
    {
    protected:
        friend class scam::MemoryManager;
        Worker(char const * id, ScamEngine * engine);

    public:
        ~Worker();

    private:
        static Worker * makeInstance(char const * id, ScamEngine * engine);

    public:
        virtual void run();
        std::string id() const;

    private:
        std::string const name;

    protected:
        ScamEngine * engine;
    };
}

#endif
