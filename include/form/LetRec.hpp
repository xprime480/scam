#if ! defined(SCAMLETREC_H)
#define SCAMLETREC_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class MemoryManager;

    class LetRec : public SpecialForm
    {
    private:
        friend class scam::MemoryManager;

        LetRec();
        static LetRec * makeInstance();

    public:
        void apply(ScamExpr * args, Continuation * cont, Env * env) override;
    };
}

#endif
