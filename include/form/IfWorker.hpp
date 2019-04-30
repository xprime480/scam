#if ! defined(IFWORKER_HPP)
#define IFWORKER_HPP 1

#include "Worker.hpp"

namespace scam
{
    class Continuation;
    class Env;
    
    class CountedListParser;

    class IfWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        IfWorker(Continuation * cont, Env * env, CountedListParser * parser);

        static IfWorker * makeInstance(Continuation * cont,
                                       Env * env,
                                       CountedListParser * parser);

    public:
        void mark() const override;
        void run() override;

    private:
        CountedListParser * parser;
        Continuation * cont;
        Env * env;
    };
}
#endif
