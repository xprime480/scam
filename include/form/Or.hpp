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
        /* I think this should be private.  Check later */
        static Or * makeInstance();

        void apply(ExprHandle args, Continuation * cont, Env * env) override;
    };
}

#endif
