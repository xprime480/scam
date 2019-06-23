#if ! defined(IFCONT_HPP)
#define IFCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class Env;
    class MemoryManager;

    class IfCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        IfCont(ScamValue args,
               Continuation * cont,
               Env * env,
               ScamEngine * engine);

        static IfCont * makeInstance(ScamValue args,
                                     Continuation * cont,
                                     Env * env,
                                     ScamEngine * engine);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        ScamValue      args;
        Continuation * cont;
        Env          * env;
    };
}

#endif
