#if ! defined(SCAMIF_H)
#define SCAMIF_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class If : public SpecialForm
    {
    public:
        If();

        void
        apply(ExprHandle const & args, ContHandle cont, Env & env) override;

        ExprHandle clone() const override;
    };
}

#endif
