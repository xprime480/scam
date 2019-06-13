#if ! defined(LETSTEPCONT_HPP)
#define LETSTEPCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LetStepCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        LetStepCont(ScamValue formals,
                    ScamValue forms,
                    ScamValue evaled,
                    ScamValue args,
                    Continuation * cont,
                    Env * env,
                    ScamEngine * engine,
                    bool rebind);

        static LetStepCont * makeInstance(ScamValue formals,
                                          ScamValue forms,
                                          ScamValue evaled,
                                          ScamValue args,
                                          Continuation * cont,
                                          Env * env,
                                          ScamEngine * engine,
                                          bool rebind);

    public:
        void mark() const override;

        void run(ScamValue expr) override;

    private:
        ScamValue formals;
        ScamValue forms;
        ScamValue evaled;
        ScamValue args;
        Continuation * cont;
        Env * env;
        bool rebind;
    };
}

#endif
