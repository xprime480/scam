#if ! defined(QQCONSLISTCDRCONT_HPP)
#define QQCONSLISTCDRCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class  QQConsListCdrCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        QQConsListCdrCont(ExprHandle car, Continuation * cont, Env * env);

        static QQConsListCdrCont *
        makeInstance(ExprHandle car, Continuation * cont, Env * env);

    public:
        void mark() const override;
        void run(ExprHandle expr) override;

    private:
        ExprHandle car;
        Continuation * cont;
        Env  *      env;

        void handle(ExprHandle expr);
        bool check_splice(ExprHandle expr);
        void do_splice(ExprHandle expr);
    };
}

#endif
