#if ! defined(EXTRACTLASTCONT_HPP)
#define EXTRACTLASTCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ExtractLastCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        ExtractLastCont(Continuation * cont);
        static ExtractLastCont * makeInstance(Continuation * cont);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        Continuation * cont;
    };
}

#endif
