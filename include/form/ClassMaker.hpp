#if ! defined(SCAMCLASSMAKER_H)
#define SCAMCLASSMAKER_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class MemoryManager;

    class ClassMaker : public SpecialForm
    {
    private:
        friend class scam::MemoryManager;

        ClassMaker();
        static ClassMaker * makeInstance();
    };

    extern void applyClassMaker(ScamValue args,
                                Continuation * cont,
                                Env * env,
                                ScamEngine * engine);
}

#endif
