#if ! defined(SCAMLAMBDA_H)
#define SCAMLAMBDA_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Lambda : public SpecialForm
    {
    public:
        Lambda();

        void
        apply(ExprHandle const & args, ContHandle cont, Env & env) override;
    };
}

#endif
