#if ! defined(SCAMDEFINE_H)
#define SCAMDEFINE_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Define : public SpecialForm
    {
    public:
        Define();

        void
        apply(ExprHandle const & args, ContHandle cont, Env & env) override;
    };
}

#endif
