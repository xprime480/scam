#if ! defined(SCAMLETSTAR_H)
#define SCAMLETSTAR_H 1

#include "form/SpecialForm.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class LetStar : public SpecialForm
    {
    private:
        friend class scam::MemoryManager;

        LetStar(ScamEngine * engine);
        static LetStar * makeInstance(ScamEngine * engine);

    public:
        static ScamValue safeCons(ScamValue expr);
    };

    extern void applyLetStar(ScamValue args,
                             Continuation * cont,
                             Env * env,
                             ScamEngine * engine);
}

#endif
