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

        QQConsListCarCont(ScamValue cdr,
                          Continuation * cont,
                          Env * env,
                          ScamEngine * engine);

        static QQConsListCarCont *
        makeInstance(ScamValue cdr,
                     Continuation * cont,
                     Env * env,
                     ScamEngine * engine);

    public:
        void mark() const override;
        void run(ScamValue expr) override;

    private:
        ScamValue cdr;
        Continuation * cont;
        Env *       env;
    };
}

#endif
