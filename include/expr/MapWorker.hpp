#if ! defined(MAPWORKER_HPP)
#define MAPWORKER_HPP 1

#include "Worker.hpp"

#include "expr/WorkerData.hpp"

namespace scam
{
    class Congtinuation;
    class Env;
    class ScamExpr;
    class MemoryManager;

    class  MapWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        MapWorker(Continuation * cont,
                  Env * env,
                  ScamExpr * car,
                  ScamExpr * cdr);

        static MapWorker * makeInstance(Continuation * cont,
                                        Env * env,
                                        ScamExpr * car,
                                        ScamExpr * cdr);

    public:
        void mark() const override;
        void run() override;

    private:
        WorkerData data;
    };
}

#endif
