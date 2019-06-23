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

        IncludeCont(ScamValue args, Continuation * cont, ScamEngine * engine);

        static IncludeCont *
        makeInstance(ScamValue args, Continuation * cont, ScamEngine * engine);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        ScamValue args;
        Continuation * cont;
    };
}

#endif
