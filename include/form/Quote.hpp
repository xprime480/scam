#if ! defined(SCAMQUOTE_H)
#define SCAMQUOTE_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Quote : public SpecialForm
    {
    private:
        Quote();

    public:
        /* I think this should be private, check later */
        static Quote * makeInstance();

        void apply(ExprHandle args, Continuation * cont, Env * env) override;
    };
}

#endif
