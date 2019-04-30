#if ! defined(MAPCDR_HPP)
#define MAPCDR_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"
#include "expr/WorkerData.hpp"

namespace scam
{
    class  MapCdr : public Worker
    {
    private:
        friend class scam::MemoryManager;

        MapCdr(ExprHandle car,
               ExprHandle cdr,
               Continuation * cont,
               Env * env);

        static MapCdr * makeInstance(ExprHandle car,
                                     ExprHandle cdr,
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
