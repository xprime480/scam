#if ! defined(SCAMAPPLY_H)
#define SCAMAPPLY_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Apply : public SpecialForm
    {
    private:
        Apply();

    public:
        /* This should probably be private */
        static Apply * makeInstance();
    };

    extern void applyApply(ScamValue args,
                           Continuation * cont,
                           Env * env,
                           ScamEngine * engine);
}

#endif
