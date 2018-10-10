#if ! defined(SCAMNOT_H)
#define SCAMNOT_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Not : public SpecialForm
    {
    public:
        Not();

        void
        apply(ExprHandle const & args, ContHandle cont, Env & env) override;
    };
}

#endif
