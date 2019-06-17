#if ! defined(DEFINEWORKER_HPP)
#define DEFINEWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"
#include "input/ArgParserFwd.hpp"

namespace scam
{
    class DefineWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        DefineWorker(DefineParser * parser,
                     Continuation * cont,
                     Env * env,
                     ScamEngine * engine);

        static DefineWorker * makeInstance(DefineParser * parser,
                                           Continuation * cont,
                                           Env * env,
                                           ScamEngine * engine);

    public:
        void mark() override;
        void run() override;

    private:
        DefineParser * parser;
        Continuation * cont;
        Env          * env;
    };
}

#endif
