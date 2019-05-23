#if ! defined(SCAMAND_H)
#define SCAMAND_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class And : public SpecialForm
    {
    private:
        And();

    public:
        static And * makeInstance();
        void apply(ScamValue args, Continuation * cont, Env * env) override;
    };
}

#endif
