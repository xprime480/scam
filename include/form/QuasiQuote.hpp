#if ! defined(SCAMQUASIQUOTE_H)
#define SCAMQUASIQUOTE_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class QuasiQuote : public SpecialForm
    {
    public:
        static ScamValue const spliceTag;

    private:
        QuasiQuote();

    public:
        /* I think this should be private, check later */
        static QuasiQuote * makeInstance();

        void apply(ScamValue args, Continuation * cont, Env * env) override;
    };
}

#endif
