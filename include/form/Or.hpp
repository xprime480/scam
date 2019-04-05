#if ! defined(SCAMOR_H)
#define SCAMOR_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Or : public SpecialForm
    {
    private:
        Or();

    public:
        static Or * makeInstance();
        void apply(ScamExpr * args, ContHandle cont, Env env) override;
    };
}

#endif
