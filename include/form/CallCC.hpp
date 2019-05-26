#if ! defined(SCAMCALLCC_H)
#define SCAMCALLCC_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class CallCC : public SpecialForm
    {
    private:
        CallCC();

    public:
        static CallCC * makeInstance();
    };

    extern void applyCallCC(ScamValue args,
                            Continuation * cont,
                            Env * env,
                            ScamEngine * engine);
}

#endif
