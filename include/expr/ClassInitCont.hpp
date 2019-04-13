#if ! defined(CLASSINITCONT_HPP)
#define CLASSINITCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class Congtinuation;
    class ScamExpr;
    class MemoryManager;

    class ClassInitCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        ClassInitCont(ScamExpr * instance, Continuation * cont);

        static ClassInitCont * makeInstance(ScamExpr * instance,
                                            Continuation * cont);

    public:
        void mark() const override;
        void run(ScamExpr * expr) override;

    private:
        ScamExpr * instance;
        Continuation * cont;
    };
}

#endif
