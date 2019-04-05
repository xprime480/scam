#if ! defined(SCAMLAMBDA_H)
#define SCAMLAMBDA_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Lambda : public SpecialForm
    {
    private:
        Lambda();

    public:
        static Lambda * makeInstance();

        void apply(ScamExpr * args, ContHandle cont, Env env) override;
    };
}

#endif
