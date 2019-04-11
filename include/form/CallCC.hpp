#if ! defined(SCAMCALLCC_H)
#define SCAMCALLCC_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class CallCC : public SpecialForm
    {
    private:
        CallCC();

    public:
        static CallCC * makeInstance();
        void apply(ScamExpr * args, Continuation * cont, Env * env) override;
    };
}

#endif
