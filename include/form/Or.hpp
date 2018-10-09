#if ! defined(SCAMOR_H)
#define SCAMOR_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Or : public SpecialForm
    {
    public:
        Or();

        void
        apply(ExprHandle const & args, ContHandle cont, Env & env) override;

        ExprHandle clone() const override;
    };
}

#endif
