#if ! defined(NOTWORKER_HPP)
#define NOTWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class SingletonParser;

    class NotWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        NotWorker(Continuation * cont,
                  Env * env,
                  ScamEngine * engine,
                  SingletonParser * parser);

        static NotWorker * makeInstance(Continuation * cont,
                                        Env * env,
                                        ScamEngine * engine,
                                        SingletonParser * parser);

    public:
        void mark() const override;
        void run() override;

    private:
        SingletonParser * parser;
        Continuation * cont;
        Env * env;
    };
}

#endif
