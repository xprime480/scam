#if ! defined(QQCONSLISTCDRCONT_HPP)
#define QQCONSLISTCDRCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class MemoryManager;
    class Env;
    class ScamExpr;

    class  QQConsListCdrCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        QQConsListCdrCont(ScamExpr * car, Continuation * cont, Env * env);

        static QQConsListCdrCont *
        makeInstance(ScamExpr * car, Continuation * cont, Env * env);

    public:
        void mark() const override;
        void run(ScamExpr * expr) override;

    private:
        ScamExpr * car;
        Continuation * cont;
        Env  *      env;

        void handle(ScamExpr * expr);
        bool check_splice(ScamExpr * expr);
        void do_splice(ScamExpr * expr);
    };
}

#endif
