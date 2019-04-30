#if ! defined(SCAMEVAL_H)
#define SCAMEVAL_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class Eval : public SpecialForm
    {
    private:
        Eval();

    public:
        static Eval * makeInstance();

        void apply(ExprHandle args, Continuation * cont, Env * env) override;
    };
}

#endif
