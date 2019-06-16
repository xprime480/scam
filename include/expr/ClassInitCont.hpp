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

        ClassInitCont(ScamValue instance,
                      Continuation * cont,
                      ScamEngine * engine);

        static ClassInitCont * makeInstance(ScamValue instance,
                                            Continuation * cont,
                                            ScamEngine * engine);

    public:
        void mark() const override;
        void handleValue(ScamValue value) override;

    private:
        ScamValue      instance;
        Continuation * cont;
    };
}

#endif
