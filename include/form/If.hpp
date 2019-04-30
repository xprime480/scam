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
        /* I think this should be private, check later */
        static If * makeInstance();

        void apply(ExprHandle args, Continuation * cont, Env * env) override;
    };
}

#endif
