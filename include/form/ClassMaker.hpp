#if ! defined(SCAMCLASSMAKER_H)
#define SCAMCLASSMAKER_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class ClassMaker : public SpecialForm
    {
    public:
        ClassMaker();

        void apply(ScamExpr * args, ContHandle cont, Env env) override;
    };
}

#endif
