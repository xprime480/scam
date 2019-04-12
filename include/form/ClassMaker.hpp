#if ! defined(SCAMCLASSMAKER_H)
#define SCAMCLASSMAKER_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class MemoryManager;

    class ClassMaker : public SpecialForm
    {
    private:
        friend class scam::MemoryManager;

        ClassMaker();
        static ClassMaker * makeInstance();

    public:
        void apply(ScamExpr * args,
                   Continuation * cont,
                   Env * env) override;

    private:
        bool validate_args(ScamExpr * args, Continuation * cont);
    };
}

#endif
