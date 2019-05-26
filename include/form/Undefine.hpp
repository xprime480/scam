#if ! defined(SCAMUNDEFINE_HPP)
#define SCAMUNDEFINE_HPP 1

#include "form/SpecialForm.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class Undefine : public SpecialForm
    {
    private:
        friend class scam::MemoryManager;
        Undefine();
        static Undefine * makeInstance();
    };

    extern void applyUndefine(ScamValue args,
                              Continuation * cont,
                              Env * env,
                              ScamEngine * engine);
}

#endif
