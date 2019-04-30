#if ! defined(CLASSINITCONT_HPP)
#define CLASSINITCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ClassInitCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        ClassInitCont(ExprHandle instance, Continuation * cont);

        static ClassInitCont *
        makeInstance(ExprHandle instance, Continuation * cont);

    public:
        void mark() const override;
        void run(ExprHandle expr) override;

    private:
        ExprHandle instance;
        Continuation * cont;
    };
}

#endif
