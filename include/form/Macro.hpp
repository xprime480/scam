#if ! defined(SCAMMACRO_H)
#define SCAMMACRO_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Macro : public SpecialForm
    {
    private:
        Macro();

    public:
        static Macro * makeInstance();
    };

    extern void applyMacro(ScamValue args,
                           Continuation * cont,
                           Env * env,
                           ScamEngine * engine);
}

#endif
