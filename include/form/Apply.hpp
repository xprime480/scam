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
        /* This should probably be private */
        static Apply * makeInstance();

        void apply(ExprHandle args, Continuation * cont, Env * env) override;
    };
}

#endif
