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
        static Quote * makeInstance();

        void apply(ScamExpr * args, Continuation * cont, Env env) override;
    };

    class QuasiQuote : public SpecialForm
    {
    private:
        QuasiQuote();

    public:
        static QuasiQuote * makeInstance();

        void apply(ScamExpr * args, Continuation * cont, Env env) override;
    };
}

#endif
