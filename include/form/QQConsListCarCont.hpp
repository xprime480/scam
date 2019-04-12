#if ! defined(QQCONSLISTCARCONT_HPP)
#define QQCONSLISTCARCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class MemoryManager;
    class Env;
    class ScamExpr;

    class  QQConsListCarCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        QQConsListCarCont(ScamExpr * cdr, Continuation * cont, Env * env);

        static QQConsListCarCont *
        makeInstance(ScamExpr * cdr, Continuation * cont, Env * env);

    public:
        void mark() const override;
        void run(ScamExpr * expr) override;

    private:
        ScamExpr * cdr;
        Continuation * cont;
        Env *       env;
    };
}

#endif
