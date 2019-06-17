#if ! defined(ORWORKER_HPP)
#define ORWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ListParser;

    class OrWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        OrWorker(Continuation * cont,
                 Env * env,
                 ListParser * parser,
                 ScamEngine * engine,
                 size_t n);

        static OrWorker * makeInstance(Continuation * cont,
                                       Env * env,
                                       ListParser * parser,
                                       ScamEngine * engine,
                                       size_t n);

    public:
        void mark() override;
        void run() override;

    private:
        ListParser * parser;
        Continuation * cont;
        Env * env;
        size_t n;
    };
}

#endif
