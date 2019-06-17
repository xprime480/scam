#if ! defined(LETEVALWORKER_HPP)
#define LETEVALWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LetEvalWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        LetEvalWorker(ScamValue formals,
                      ScamValue evaled,
                      ScamValue args,
                      ScamValue forms,
                      Continuation * cont,
                      Env * env,
                      ScamEngine * engine,
                      bool rebind);

        static LetEvalWorker * makeInstance(ScamValue formals,
                                            ScamValue evaled,
                                            ScamValue args,
                                            ScamValue forms,
                                            Continuation * cont,
                                            Env * env,
                                            ScamEngine * engine,
                                            bool rebind);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue formals;
        ScamValue evaled;
        ScamValue args;
        ScamValue forms;
        Continuation * cont;
        Env * env;
        bool rebind;
    };
}

#endif
