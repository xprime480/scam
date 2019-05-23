#if ! defined(CLASSINITCONT_HPP)
#define CLASSINITCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ScamInstance;

    class ClassInitCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        ClassInitCont(ScamInstance * instance, Continuation * cont);

        static ClassInitCont *
        makeInstance(ScamInstance * instance, Continuation * cont);

    public:
        void mark() const override;
        void run(ScamValue expr) override;

    private:
        ScamInstance * instance;
        Continuation * cont;
    };
}

#endif
