#if ! defined(MAPWORKER_HPP)
#define MAPWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"
#include "expr/WorkerData.hpp"

namespace scam
{
    class  MapWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        MapWorker(Continuation * cont,
                  Env * env,
                  ExprHandle car,
                  ExprHandle cdr);

        static MapWorker * makeInstance(Continuation * cont,
                                        Env * env,
                                        ExprHandle car,
                                        ExprHandle cdr);

    public:
        void mark() const override;
        void run() override;

    private:
        WorkerData data;
    };
}

#endif
