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

        ApplyArgsCont(ScamValue op,
                      Continuation * cont,
                      Env * env,
                      ScamEngine * engine);

        static ApplyArgsCont * makeInstance(ScamValue op,
                                            Continuation * cont,
                                            Env * env,
                                            ScamEngine * engine);

    public:
        void mark() const override;
        void run(ScamValue expr) override;

    private:
        ScamValue op;
        Continuation * cont;
        Env * env;
    };
}

#endif
