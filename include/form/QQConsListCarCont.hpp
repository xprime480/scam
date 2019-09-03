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

        QQConsListCarCont(ScamValue cdr, Continuation * cont, Env * env);

        static QQConsListCarCont *
        makeInstance(ScamValue cdr, Continuation * cont, Env * env);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        ScamValue cdr;
        Continuation * cont;
        Env *       env;
    };
}

#endif
