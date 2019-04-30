#if ! defined(INCLUDEWORKER_HPP)
#define INCLUDEWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class IncludeWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        IncludeWorker(ExprHandle args,
                      Continuation * cont,
                      ScamEngine * engine);

        static IncludeWorker * makeInstance(ExprHandle args,
                                            Continuation * cont,
                                            ScamEngine * engine);

    public:
        void mark() const override;
        void run() override;

    private:
        ExprHandle args;
        Continuation * cont;
        ScamEngine * engine;
    };
}

#endif
