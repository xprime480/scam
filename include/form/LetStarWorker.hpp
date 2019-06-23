#if ! defined(LETSTARWORKER_HPP)
#define LETSTARWORKER_HPP 1

#include "LetBaseWorker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LetDef;

    class LetStarWorker : public LetBaseWorker
    {
    private:
        friend class scam::MemoryManager;
        LetStarWorker(LetDef & def,
                      Continuation * cont,
                      Env * env,
                      ScamEngine * engine);

        static LetStarWorker * makeInstance(LetDef & def,
                                            Continuation * cont,
                                            Env * env,
                                            ScamEngine * engine);

    protected:
        void do_next(ScamValue formals,
                     ScamValue values,
                     ScamValue forms) override;
    };
}

#endif
