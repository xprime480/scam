#if ! defined(UNDEFINEWORKER_HPP)
#define UNDEFINEWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class UndefineParser;

    class UndefineWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        UndefineWorker(UndefineParser * parser,
                       Continuation * cont,
                       Env * env);

        static UndefineWorker * makeInstance(UndefineParser * parser,
                                             Continuation * cont,
                                             Env * env);

    public:
        void mark() const override;

        void run() override;

    private:
        UndefineParser * parser;
        Continuation   * cont;
        Env            * env;
    };
}

#endif
