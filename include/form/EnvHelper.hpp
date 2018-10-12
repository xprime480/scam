#if ! defined(SCAMENVHELPER_H)
#define SCAMENVHELPER_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class EnvHelper : public SpecialForm
    {
    public:
        EnvHelper(char const * name);
    };

    class Define : public EnvHelper
    {
    public:
        Define();

        void
        apply(ExprHandle const & args, ContHandle cont, Env & env) override;
    };

    class Assign : public EnvHelper
    {
    public:
        Assign();

        void
        apply(ExprHandle const & args, ContHandle cont, Env & env) override;
    };
}

#endif
