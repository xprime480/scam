#if ! defined(SCAMQUOTE_H)
#define SCAMQUOTE_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Quote : public SpecialForm
    {
    private:
        Quote();

    public:
        /* I think this should be private, check later */
        static Quote * makeInstance();
    };

    extern void applyQuote(ScamValue args,
                           Continuation * cont,
                           Env * env,
                           ScamEngine * engine);
}

#endif
