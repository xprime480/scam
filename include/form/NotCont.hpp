#if ! defined(NOTCONT_HPP)
#define NOTCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class MemoryManager;
    
    class NotCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        NotCont(Continuation * cont);
        static NotCont * makeInstance(Continuation * cont);

    public:
        void mark() const override;
        void run(ScamValue expr) override;

    private:
        Continuation * cont;
    };
}

#endif
