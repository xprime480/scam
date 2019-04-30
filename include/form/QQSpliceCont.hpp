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
        QQSpliceCont(Continuation * cont);
        static QQSpliceCont * makeInstance(Continuation * cont);

    public:
        void mark() const override;
        void run(ExprHandle expr) override;

    private:
        Continuation * cont;
    };
}

#endif
