#if ! defined(SCAMQUASIQUOTE_H)
#define SCAMQUASIQUOTE_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class QuasiQuote : public SpecialForm
    {
    public:
        static ScamExpr * const spliceTag;

    private:
        QuasiQuote();

    public:
        static QuasiQuote * makeInstance();

        void apply(ScamExpr * args, Continuation * cont, Env * env) override;

        static void qq_apply(ScamExpr * args,
                             Continuation * cont,
                             Env * env,
                             bool top);
    };
}

#endif
