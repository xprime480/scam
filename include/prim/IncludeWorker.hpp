#if ! defined(INCLUDEWORKER_HPP)
#define INCLUDEWORKER_HPP 1

#include "Worker.hpp"

namespace scam
{
    class Continuation;
    class MemoryManager;
    class ScamEngine;
    class ScamExpr;

    class IncludeWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        IncludeWorker(ScamExpr * args,
                      Continuation * cont,
                      ScamEngine * engine);

        static IncludeWorker * makeInstance(ScamExpr * args,
                                            Continuation * cont,
                                            ScamEngine * engine);

    public:
        void mark() const override;
        void run() override;

    private:
        ScamExpr * args;
        Continuation * cont;
        ScamEngine * engine;
    };
}

#endif
