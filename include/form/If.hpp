#if ! defined(SCAMIF_H)
#define SCAMIF_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class If : public SpecialForm
    {
    private:
        If();

    public:
        static If * makeInstance();
        void apply(ScamExpr * args, Continuation * cont, Env * env) override;
    };
}

#endif
