#if ! defined(APPLYARGSCONT_HPP)
#define APPLYARGSCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ApplyArgsCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        ApplyArgsCont(ScamValue op, Continuation * cont, Env * env);

        static ApplyArgsCont *
        makeInstance(ScamValue op, Continuation * cont, Env * env);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        ScamValue op;
        Continuation * cont;
        Env * env;
    };
}

#endif
