#if ! defined(SCAMLETREC_H)
#define SCAMLETREC_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class MemoryManager;

    class LetRec : public SpecialForm
    {
    private:
        friend class scam::MemoryManager;

        LetRec();
        static LetRec * makeInstance();
    };

    extern void applyLetRec(ScamValue args,
                            Continuation * cont,
                            Env * env,
                            ScamEngine * engine);
}

#endif
