#if ! defined(SCAMAND_H)
#define SCAMAND_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class And : public SpecialForm
    {
    private:
        And();

    public:
        static And * makeInstance();
    };

    extern void applyAnd(ScamValue args,
                         Continuation * cont,
                         Env * env,
                         ScamEngine * engine);

}

#endif
