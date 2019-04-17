#if ! defined(INCLUDECONT_HPP)
#define INCLUDECONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class MemoryManager;
    class ScamEngine;
    class ScamExpr;

    class IncludeCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        IncludeCont(ScamExpr * rest, Continuation * cont, ScamEngine * engine);

        static IncludeCont *
        makeInstance(ScamExpr * rest, Continuation * cont, ScamEngine * engine);

    public:
        void mark() const override;
        void run(ScamExpr * expr) override;

    private:
        ScamExpr * rest;
        Continuation * cont;
        ScamEngine * engine;
    };
}

#endif
