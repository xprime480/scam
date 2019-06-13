#if ! defined(QQSPLICECONT_HPP)
#define QQSPLICECONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class MemoryManager;

    class  QQSpliceCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        QQSpliceCont(Continuation * cont, ScamEngine * engine);

        static QQSpliceCont *
        makeInstance(Continuation * cont, ScamEngine * engine);

    public:
        void mark() const override;
        void run(ScamValue expr) override;

    private:
        Continuation * cont;
    };
}

#endif
