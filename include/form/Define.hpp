#if ! defined(SCAMDEFINE_HPP)
#define SCAMDEFINE_HPP 1

#include "form/SpecialForm.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class Define : public SpecialForm
    {
    private:
        friend class scam::MemoryManager;
        Define(ScamEngine * engine);
        static Define * makeInstance(ScamEngine * engine);
    };

    extern void applyDefine(ScamValue args,
                            Continuation * cont,
                            Env * env,
                            ScamEngine * engine);
}

#endif
