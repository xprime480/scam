#if ! defined(MACROEVALCONT_HPP)
#define MACROEVALCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class Env;
    class ScamExpr;
    class MemoryManager;

    class MacroEvalCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        MacroEvalCont(Continuation * cont, Env * capture);

        static MacroEvalCont *
        makeInstance(Continuation * cont, Env * capture);

    public:
        void mark() const override;
        void run(ScamExpr * expr) override;

    private:
        Continuation * cont;
        Env *        capture;
    };
}

#endif