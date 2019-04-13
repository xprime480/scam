#if ! defined(MAPCDR_HPP)
#define MAPCDR_HPP 1

#include "Worker.hpp"

#include "expr/WorkerData.hpp"

namespace scam
{
    class Congtinuation;
    class Env;
    class ScamExpr;
    class MemoryManager;

    class  MapCdr : public Worker
    {
    private:
        friend class scam::MemoryManager;
        MapCdr(ScamExpr * car,
               ScamExpr * cdr,
               Continuation * cont,
               Env * env);

        static MapCdr * makeInstance(ScamExpr * car,
                                     ScamExpr * cdr,
                                     Continuation * cont,
                                     Env * env);

    public:
        void mark() const override;
        void run() override;

    private:
        WorkerData data;
    };
}

#endif
