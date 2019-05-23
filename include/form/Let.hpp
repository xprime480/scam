#if ! defined(SCAMLET_H)
#define SCAMLET_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class MemoryManager;

    class Let : public SpecialForm
    {
    private:
        friend class scam::MemoryManager;

        Let();
        static Let * makeInstance();

    public:
        void apply(ScamValue args, Continuation * cont, Env * env) override;
    };
}

#endif
