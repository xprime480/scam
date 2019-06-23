#if ! defined(LETWORKER_HPP)
#define LETWORKER_HPP 1

#include "LetBaseWorker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LetDef;

    class LetWorker : public LetBaseWorker
    {
    private:
        friend class scam::MemoryManager;

        LetWorker(LetDef & parser,
                  Continuation * cont,
                  Env * env,
                  ScamEngine * engine,
                  bool rebind);

        static LetWorker * makeInstance(LetDef & parser,
                                        Continuation * cont,
                                        Env * env,
                                        ScamEngine * engine,
                                        bool rebind);

    protected:
        void do_next(ScamValue formals,
                     ScamValue values,
                     ScamValue forms) override;

    private:
        const bool rebind;
    };
}

#endif
