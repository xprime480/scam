#if ! defined(MACROEVALCONT_HPP)
#define MACROEVALCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class MacroEvalCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        MacroEvalCont(Continuation * cont, Env * capture, ScamEngine * engine);

        static MacroEvalCont * makeInstance(Continuation * cont,
                                            Env * capture,
                                            ScamEngine * engine);

    public:
        void mark() const override;
        void run(ScamValue expr) override;

    private:
        Continuation * cont;
        Env *        capture;
    };
}

#endif
