#if ! defined(DEFINEWORKER_HPP)
#define DEFINEWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class AssignParser;

    class DefineWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        DefineWorker(AssignParser * parser,
                     Continuation * cont,
                     Env * env,
                     ScamEngine * engine);

        static DefineWorker * makeInstance(AssignParser * parser,
                                           Continuation * cont,
                                           Env * env,
                                           ScamEngine * engine);

    public:
        void mark() const override;
        void run() override;

    private:
        AssignParser * parser;
        Continuation * cont;
        Env          * env;
        ScamEngine   * engine;
    };
}

#endif
