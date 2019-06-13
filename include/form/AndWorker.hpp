#if ! defined(ANDWORKER_HPP)
#define ANDWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ListParser;

    class AndWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        AndWorker(Continuation * cont,
                  Env * env,
                  ListParser * parser,
                  ScamEngine * engine,
                  size_t n);

        static AndWorker * makeInstance(Continuation * cont,
                                        Env * env,
                                        ListParser * parser,
                                        ScamEngine * engine,
                                        size_t n);

    public:
        void mark() const override;
        void run() override;

    private:
        Continuation * cont;
        Env * env;
        ListParser * parser;
        size_t n;
    };
}

#endif
