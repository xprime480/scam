#if ! defined(INCLUDECONT_HPP)
#define INCLUDECONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class IncludeCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        IncludeCont(ExprHandle rest, Continuation * cont, ScamEngine * engine);

        static IncludeCont *
        makeInstance(ExprHandle rest, Continuation * cont, ScamEngine * engine);

    public:
        void mark() const override;
        void run(ExprHandle expr) override;

    private:
        ExprHandle rest;
        Continuation * cont;
        ScamEngine * engine;
    };
}

#endif
