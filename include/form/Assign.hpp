#if ! defined(SCAMASSIGN_HPP)
#define SCAMASSIGN_HPP 1

#include "form/SpecialForm.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class Assign : public SpecialForm
    {
    private:
        friend class scam::MemoryManager;
        Assign(ScamEngine * engine);
        static Assign * makeInstance(ScamEngine * engine);

    public:
        void apply(ScamValue args, Continuation * cont, Env * env) override;

    private:
        ScamEngine * engine;
    };
}

#endif
