#if ! defined(SCAMAPPLY_H)
#define SCAMAPPLY_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Apply : public SpecialForm
    {
      private:
        Apply();

    public:
        static Apply * makeInstance();
        void apply(ScamExpr * args, ContHandle cont, Env env) override;
    };
}

#endif
