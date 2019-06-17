#if ! defined(ASSIGNWORKER_HPP)
#define ASSIGNWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"
#include "input/ArgParserFwd.hpp"

namespace scam
{
    class AssignWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        AssignWorker(AssignParser * parser,
                     Continuation * cont,
                     Env * env,
                     ScamEngine * engine);

        static AssignWorker * makeInstance(AssignParser * parser,
                                           Continuation * cont,
                                           Env * env,
                                           ScamEngine * engine);

    public:
        void mark() override;
        void run() override;

    private:
        AssignParser * parser;
        Continuation * cont;
        Env          * env;
    };
}

#endif
