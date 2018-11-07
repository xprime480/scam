#if ! defined(SCAMMACRO_H)
#define SCAMMACRO_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Macro : public SpecialForm
    {
    public:
        Macro();

        void apply(ScamExpr * args, ContHandle cont, Env env) override;
    };
}

#endif
