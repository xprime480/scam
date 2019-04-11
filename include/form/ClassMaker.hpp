#if ! defined(SCAMCLASSMAKER_H)
#define SCAMCLASSMAKER_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class ClassMaker : public SpecialForm
    {
    private:
        ClassMaker();

    public:
        static ClassMaker * makeInstance();
        void apply(ScamExpr * args, Continuation * cont, Env * env) override;
    };
}

#endif
