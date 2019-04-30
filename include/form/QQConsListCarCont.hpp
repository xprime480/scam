#if ! defined(QQCONSLISTCARCONT_HPP)
#define QQCONSLISTCARCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class  QQConsListCarCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        QQConsListCarCont(ExprHandle cdr, Continuation * cont, Env * env);

        static QQConsListCarCont *
        makeInstance(ExprHandle cdr, Continuation * cont, Env * env);

    public:
        void mark() const override;
        void run(ExprHandle expr) override;

    private:
        ExprHandle cdr;
        Continuation * cont;
        Env *       env;
    };
}

#endif
