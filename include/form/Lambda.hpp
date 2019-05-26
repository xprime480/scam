#if ! defined(SCAMLAMBDA_H)
#define SCAMLAMBDA_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Lambda : public SpecialForm
    {
    private:
        Lambda();

    public:
        /* I think this should be private, check later */
        static Lambda * makeInstance();
    };

    extern void applyLambda(ScamValue args,
                            Continuation * cont,
                            Env * env,
                            ScamEngine * engine);
}

#endif
