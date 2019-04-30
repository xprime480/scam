#if ! defined(SCAMNOT_H)
#define SCAMNOT_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Not : public SpecialForm
    {
    private:
        Not();

    public:
        static Not * makeInstance();

        void apply(ExprHandle args, Continuation * cont, Env * env) override;
    };
}

#endif
