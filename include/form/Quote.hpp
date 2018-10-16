#if ! defined(SCAMQUOTE_H)
#define SCAMQUOTE_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Quote : public SpecialForm
    {
    public:
        Quote();

        void apply(ScamExpr * args, ContHandle cont, Env env) override;
    };
}

#endif
